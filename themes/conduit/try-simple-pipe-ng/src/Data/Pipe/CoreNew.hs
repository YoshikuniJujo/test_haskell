{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Pipe.CoreNew (

	-- * DATA PIPE

	Pipe(..),

	-- * RUN

	runPipe, runPipe_,

	-- * PIPE FUNCTIONS

	await, yield, convert, (=$=), (=@=),

	-- * MAP

	mapMonad, mapIn, mapOut,

	-- * FINALIZERS

	bracket, finally, finalize, onBreak, onDone,

	-- * CHOICE

	appLeft, appRight, (++++), (||||),

	) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.MonadClasses.Reader
import Control.MonadClasses.Writer
import Control.MonadClasses.State
import Control.MonadClasses.Except
import Control.Exception.Lifted (onException)

data Pipe i o m r
	= Ready (m ()) o (Pipe i o m r) | Need (m ()) (Maybe i -> Pipe i o m r)
	| Done (m ()) r | Make (m ()) (m (Pipe i o m r))

{-
data Pipe i o m r
	= Ready o (Pipe i o m r)
	| Need (Maybe i -> Pipe i o m r)
	| Done r
	| Make (m (Pipe i o m r))
	-}

finalizer :: Pipe i o m r -> m ()
finalizer = \case Ready f _ _ -> f; Need f _ -> f; Done f _ -> f; Make f _ -> f

runPipe :: Monad m => Pipe i o m r -> m (Maybe r)
runPipe = \case
	Done f r -> Just r <$ f; Make _ m -> runPipe =<< m; _ -> pure Nothing

runPipe_ :: Monad m => Pipe i o m r -> m ()
runPipe_ = void . runPipe

await :: Monad m => Pipe i o m (Maybe i)
await = Need (pure ()) pure

yield :: Monad m => o -> Pipe i o m ()
yield x = Ready (pure ()) x (pure ())

convert :: Monad m => (a -> b) -> Pipe a b m ()
convert f = maybe (pure ()) ((>> convert f) . yield . f) =<< await

infixr 3 =$=

(=$=) :: Monad m => Pipe i a m r -> Pipe a o m r' -> Pipe i o m r'
p =$= Make f m = Make f $ (p =$=) <$> m
p =$= Done f r = Done (finalizer p >> f) r
p =$= Ready f o p' = Ready f o $ p =$= p'
Need f n =$= p = Need f $ \i -> n i =$= p
Ready _ o p =$= Need _ n = p =$= n (Just o)
Done f r =$= Need f' n = Done (pure ()) r =$= Make f' (n Nothing <$ f)
Make f m =$= p = Make f $ (=$= p) <$> m

infixr 2 =@=

(=@=) :: Monad m => Pipe i a m r -> Pipe a o m r' -> Pipe i o m r
p1 =@= p2 = pss p1 =$= rcv p2
	where
	pss s = yield . Right =<< mapOut Left s
	rcv p = (p >> pure undefined) ||||
		(await >>= maybe (pure undefined) pure)

mapMonad ::
	Functor m => (forall a . m a -> m' a) -> Pipe i o m r -> Pipe i o m' r
mapMonad k = \case
	Ready f o p -> Ready (k f) o $ mapMonad k p
	Need f n -> Need (k f) $ \i -> mapMonad k $ n i
	Done f r -> Done (k f) r; Make f m -> Make (k f) . k $ mapMonad k <$> m

mapIn :: Monad m => (i' -> i) -> Pipe i o m r -> Pipe i' o m r
mapIn k = \case
	Ready f o p -> Ready f o $ mapIn k p
	Need f p -> Need f $ \i -> mapIn k (p $ k <$> i)
	Done f r -> Done f r; Make f m -> Make f $ mapIn k <$> m

mapOut :: Monad m => (o -> o') -> Pipe i o m r -> Pipe i o' m r
mapOut k = \case
	Ready f o p -> Ready f (k o) $ mapOut k p
	Need f p -> Need f $ \i -> mapOut k (p i)
	Done f r -> Done f r; Make f m -> Make f $ mapOut k <$> m

bracket :: MonadBaseControl IO m =>
	m a -> (a -> m b) -> (a -> Pipe i o m r) -> Pipe i o m r
bracket o c p = lift o >>= \h -> p h `finally` void (c h)

finally :: MonadBaseControl IO m => Pipe i o m r -> m b -> Pipe i o m r
p `finally` f = finalize (mapMonad (`onException` f) p) f

finalize :: Monad m => Pipe i o m r -> m b -> Pipe i o m r
p `finalize` f = p `onBreak` f `onDone` f

onBreak :: Monad m => Pipe i o m r -> m b -> Pipe i o m r
onBreak (Ready f0 o p) f = Ready (f0 >> void f) o $ onBreak p f
onBreak (Need f0 n) f = Need (f0 >> void f) $ \i -> onBreak (n i) f
onBreak (Done f0 r) _ = Done f0 r
onBreak (Make f0 m) f = Make (f0 >> void f) $ (`onBreak` f) <$> m

onDone :: Monad m => Pipe i o m r -> m b -> Pipe i o m r
onDone (Ready f0 o p) f = Ready f0 o $ p `finalize` f
onDone (Need f0 n) f = Need f0 $ \i -> n i `finalize` f
onDone (Done f0 r) f = Done (f0 >> void f) r
onDone (Make f0 m) f = Make f0 $ (`finalize` f) <$> m

appLeft :: Monad m => Pipe b c m r -> Pipe (Either b d) (Either c d) m r
appLeft = \case
	Ready f o p -> Ready f (Left o) $ appLeft p
	Need f p -> Need f $ \mei -> case mei of
		Just (Left i) -> appLeft . p $ Just i
		Just (Right i) -> yield (Right i) >> appLeft (Need f p)
		_ -> appLeft $ p Nothing
	Done f r -> Done f r
	Make f p -> Make f $ appLeft <$> p

appRight :: Monad m => Pipe b c m r -> Pipe (Either d b) (Either d c) m r
appRight f = mapIn mrr . mapOut mrr $ appLeft f where mrr = either Right Left

infixr 4 ++++, ||||

(++++) :: Monad m =>
	Pipe b c m r -> Pipe b' c' m r -> Pipe (Either b b') (Either c c') m r
f ++++ g = appLeft f =$= appRight g

(||||) :: Monad m => Pipe b d m r -> Pipe c d m r -> Pipe (Either b c) d m r
f |||| g = either id id `mapOut` (f ++++ g)

instance Monad m => Monad (Pipe i o m) where
	Ready f o p >>= k = Ready f o $ p >>= k
	Need f n >>= k = Need f $ n >=> k
	Done _ r >>= k = k r; Make f m >>= k = Make f $ (>>= k) <$> m

instance MonadTrans (Pipe i o) where lift = liftP
instance Monad m => Functor (Pipe i o m) where fmap = (=<<) . (pure .)

instance Monad m => Applicative (Pipe i o m) where
	(<*>) = ap; pure = Done (pure ())

liftP :: Monad m => m a -> Pipe i o m a
liftP m = Make (pure ()) $ Done (pure ()) <$> m

instance (Monad m, MonadReader r m) => MonadReader r (Pipe i o m) where
	ask = lift ask; local = mapPipeM . local; reader = lift . reader

mapPipeM :: Monad m =>
	(m (Pipe i o m a) -> m (Pipe i o m a)) -> Pipe i o m a -> Pipe i o m a
mapPipeM k = \case
	Ready f o p -> Ready f o $ mapPipeM k p
	Need f p -> Need f $ mapPipeM k . p
	Done f r -> Done f r; Make f p -> Make f $ mapPipeM k <$> k p

instance (Monad m, MonadWriter w m) => MonadWriter w (Pipe i o m) where
	tell = lift . tell
	listen = \case
		Ready f o p -> Ready f o $ listen p
		Need f p -> Need f $ listen . p
		Done f r -> Done f (r, mempty)
		Make f p -> Make f $ (\(p', l) -> (, l) <$> p') <$> listen p
	pass = \case
		Ready f o p -> Ready f o $ pass p
		Need f p -> Need f $ pass . p
		Done f (r, _) -> Done f r; Make f p -> Make f $ pass <$> p

instance MonadState s m => MonadState s (Pipe i o m) where
	get = lift get; put = lift . put

instance MonadError e m => MonadError e (Pipe i o m) where
	throwError e = Make (pure ()) $ throwError e
	Ready f o p `catchError` h = Ready f o $ p `catchError` h
	Need f p `catchError` h = Need f $ (`catchError` h) . p
	Done f r `catchError` _ = Done f r
	Make f p `catchError` h =
		Make f . ((`catchError` h) <$>) $ p `catchError` (pure . h)

instance MonadIO m => MonadIO (Pipe i o m) where liftIO = lift . liftIO

instance MonadBase io m => MonadBase io (Pipe i o m) where
	liftBase = lift . liftBase

instance MonadFail m => MonadFail (Pipe i o m) where fail = lift . fail
