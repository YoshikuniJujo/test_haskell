{-# LANGUAGE TupleSections, TypeFamilies, FlexibleContexts, RankNTypes,
	PackageImports #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Pipe.Core (
	PipeClass(..), PipeChoice(..), (=@=), runPipe_, convert,
	Pipe(..), finally, bracket ) where

import Control.Monad
import Control.Exception.Lifted (onException)
import Control.Monad.Trans.Control
import "monads-tf" Control.Monad.Trans
import "monads-tf" Control.Monad.Except
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Reader
import "monads-tf" Control.Monad.Writer

infixr 2 =@=
infixr 3 =$=
infixr 4 ++++, ||||

class PipeClass p where
	runPipe :: Monad m => p i o m r -> m (Maybe r)
	(=$=) :: Monad m => p a b m x -> p b c m y -> p a c m y
	yield :: Monad m => o -> p i o m ()
	await :: Monad m => p i o m (Maybe i)
	onBreak :: Monad m => p i o m r -> m b -> p i o m r
	onDone :: Monad m => p i o m r -> m b -> p i o m r
	finalize :: Monad m => p i o m r -> m b -> p i o m r
	mapMonad :: Monad m => (forall a . m a -> m a) -> p i o m r -> p i o m r
	mapOut :: Monad m => (o -> o') -> p i o m r -> p i o' m r
	mapIn :: Monad m => (i' -> i) -> p i o m r -> p i' o m r

	p `finalize` f = p `onBreak` f `onDone` f

runPipe_ :: (PipeClass p, Monad m) => p i o m r -> m ()
runPipe_ = (>> return ()) . runPipe

convert :: (PipeClass p, Monad m, Monad (p a b m)) => (a -> b) -> p a b m ()
convert f = await >>= maybe (return ()) ((>> convert f) . yield . f)

-- | Minimal complete definition: 'appLeft'

class PipeClass pc => PipeChoice pc where
	appLeft :: Monad m => pc b c m r -> pc (Either b d) (Either c d) m r
	appRight :: Monad m => pc b c m r -> pc (Either d b) (Either d c) m r
	(++++) :: Monad m =>
		pc b c m r -> pc b' c' m r -> pc (Either b b') (Either c c') m r
	(||||) :: Monad m => pc b d m r -> pc c d m r -> pc (Either b c) d m r

	appRight f = mapIn mirror . mapOut mirror $ appLeft f
		where
		mirror (Left x) = Right x
		mirror (Right y) = Left y
	f ++++ g = appLeft f =$= appRight g
	f |||| g = mapOut untag (f ++++ g)
		where
		untag (Left x) = x
		untag (Right y) = y

data Pipe i o m r
	= Ready (m ()) o (Pipe i o m r)
	| Need (m ()) (Maybe i -> Pipe i o m r)
	| Done (m ()) r
	| Make (m ()) (m (Pipe i o m r))

instance PipeChoice Pipe where
	appLeft (Ready f o p) = Ready f (Left o) $ appLeft p
	appLeft (Need f p) = Need f $ \mei -> case mei of
		Just (Left i) -> appLeft . p $ Just i
		Just (Right i) -> yield (Right i) >> appLeft (Need f p)
		_ -> appLeft $ p Nothing
	appLeft (Done f r) = Done f r
	appLeft (Make f p) = Make f $ appLeft `liftM` p

instance MonadWriter m => MonadWriter (Pipe i o m) where
	type WriterType (Pipe i o m) = WriterType m
	tell = lift . tell
	listen (Ready f o p) = Ready f o $ listen p
	listen (Need f p) = Need f $ \mi -> listen $ p mi
	listen (Done f r) = Done f (r, mempty)
	listen (Make f p) = Make f $ do
		(p', l) <- listen p
		return $ (, l) `liftM` p'
	pass (Ready f o p) = Ready f o $ pass p
	pass (Need f p) = Need f $ \mi -> pass $ p mi
	pass (Done f (r, _)) = Done f r
	pass (Make f p) = Make f $ do
		pass `liftM` p

mapPipeM :: Monad m =>
	(m (Pipe i o m a) -> m (Pipe i o m a)) -> Pipe i o m a -> Pipe i o m a
mapPipeM m (Ready f o p) = Ready f o $ mapPipeM m p
mapPipeM m (Need f p) = Need f $ \mi -> mapPipeM m $ p mi
mapPipeM _ (Done f r) = Done f r
mapPipeM m (Make f p) = Make f $ mapPipeM m `liftM` m p

instance MonadError m => MonadError (Pipe i o m) where
	type ErrorType (Pipe i o m) = ErrorType m
	throwError e = Make (return ()) $ throwError e
	Ready f o p `catchError` c = Ready f o $ p `catchError` c
	Need f p `catchError` c = Need f $ \mi -> p mi `catchError` c
	Done f r `catchError` _ = Done f r
	Make f p `catchError` c =
		Make f . ((`catchError` c) `liftM`) $ p `catchError` (return . c)

finalizer :: Pipe i o m r -> m ()
finalizer (Ready f _ _) = f
finalizer (Need f _) = f
finalizer (Done f _) = f
finalizer (Make f _) = f

instance PipeClass Pipe where

	runPipe (Done f r) = f >> return (Just r)
	runPipe (Make _ m) = runPipe =<< m
	runPipe _ = return Nothing

	p =$= Make f m = Make f $ (p =$=) `liftM` m
	p =$= Done f r = Done (finalizer p >> f) r
	p =$= Ready f o p' = Ready f o $ p =$= p'
	Need f n =$= p = Need f $ \i -> n i =$= p
	Ready _ o p =$= Need _ n = p =$= n (Just o)
	Done f r =$= Need f' n =
		Done (return ()) r =$= Make f' (f >> return (n Nothing))
	Make f m =$= p = Make f $ (=$= p) `liftM` m

	yield x = Ready (return ()) x (return ())
	await = Need (return ()) return

	onBreak (Ready f0 o p) f = Ready (f0 >> f >> return ()) o $ onBreak p f
	onBreak (Need f0 n) f = Need (f0 >> f >> return ()) $ \i -> onBreak (n i) f
	onBreak (Done f0 r) _ = Done f0 r
	onBreak (Make f0 m) f = Make (f0 >> f >> return ()) $ flip onBreak f `liftM` m

	onDone (Ready f0 o p) f = Ready (voidM f0) o $ finalize p f
	onDone (Need f0 n) f = Need (voidM f0) $ \i -> finalize (n i) f
	onDone (Done f0 r) f = Done (f0 >> f >> return ()) r
	onDone (Make f0 m) f = Make (voidM f0) $ flip finalize f `liftM` m

	finalize (Ready f0 o p) f = Ready (f0 >> f >> return ()) o $ finalize p f
	finalize (Need f0 n) f = Need (f0 >> f >> return ()) $ \i -> finalize (n i) f
	finalize (Done f0 r) f = Done (f0 >> f >> return ()) r
	finalize (Make f0 m) f = Make (f0 >> f >> return ()) $ flip finalize f `liftM` m

	mapMonad k (Ready f o p) = Ready f o $ mapMonad k p
	mapMonad k (Need f n) = Need f $ \i -> mapMonad k $ n i
	mapMonad _ (Done f r) = Done f r
	mapMonad k (Make f m) = Make f . k $ mapMonad k `liftM` m

	mapOut c (Ready f o p) = Ready f (c o) $ mapOut c p
	mapOut c (Need f p) = Need f $ \i -> mapOut c (p i)
	mapOut _ (Done f r) = Done f r
	mapOut c (Make f m) = Make f $ mapOut c `liftM` m

	mapIn c (Ready f o p) = Ready f o $ mapIn c p
	mapIn c (Need f p) = Need f $ \i -> mapIn c (p $ c <$> i)
	mapIn _ (Done f r) = Done f r
	mapIn c (Make f m) = Make f $ mapIn c `liftM` m

instance Monad m => Monad (Pipe i o m) where
	Ready f o p >>= k = Ready f o $ p >>= k
	Need f n >>= k = Need f $ n >=> k
--	Done f r >>= k = Make (return ()) $ f >> return (k r)
	Done _ r >>= k = k r
	Make f m >>= k = Make f $ (>>= k) `liftM` m

instance Monad m => Functor (Pipe i o m) where
	fmap = (=<<) . (return .)

instance Monad m => Applicative (Pipe i o m) where
	(<*>) = ap
	pure = Done (return ())

instance MonadTrans (Pipe i o) where
	lift = liftP

instance MonadIO m => MonadIO (Pipe i o m) where
	liftIO = lift . liftIO

instance MonadState m => MonadState (Pipe i o m) where
	type StateType (Pipe i o m) = StateType m
	get = lift get
	put = lift . put

instance MonadReader m => MonadReader (Pipe i o m) where
	type EnvType (Pipe i o m) = EnvType m
	ask = lift ask
	local = mapPipeM . local

liftP :: Monad m => m a -> Pipe i o m a
liftP m = Make (return ()) $ Done (return ()) `liftM` m

bracket :: (MonadBaseControl IO m, PipeClass p, MonadTrans (p i o), Monad (p i o m)) =>
	m a -> (a -> m b) -> (a -> p i o m r) -> p i o m r
bracket o c p = do
	h <- lift o
	p h `finally` void (c h)

finally :: (MonadBaseControl IO m, PipeClass p) => p i o m r -> m b -> p i o m r
finally p f = finalize (mapMonad (`onException` f) p) f

voidM :: Monad m => m a -> m ()
voidM = (>> return ())

passResult :: (PipeClass p, Monad m, Monad (p i (Either a r) m)) =>
	p i a m r -> p i (Either a r) m ()
passResult s = mapOut Left s >>= yield . Right

recvResult :: (PipeChoice p, Monad m, Monad (p a o m), Monad (p r o m) ) =>
	p a o m r' -> p (Either a r) o m r
recvResult p =
	(p >> return undefined) |||| (await >>= maybe (return undefined) return)


(=@=) :: (
	PipeChoice p, Monad m,
	Monad (p i (Either a r) m), Monad (p a o m), Monad (p r o m)) =>
	p i a m r -> p a o m r' -> p i o m r
p1 =@= p2 = passResult p1 =$= recvResult p2
