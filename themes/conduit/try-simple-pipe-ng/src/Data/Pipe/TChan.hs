{-# LANGUAGE TupleSections, FlexibleContexts, PackageImports #-}

module Data.Pipe.TChan (
	fromTChan, toTChan, fromTChans, toTChans, toTChansM) where

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.Trans
import Control.Monad.Base
import Control.Concurrent.STM
import Data.List
import Data.Pipe

fromTChan :: (PipeClass p, MonadBase IO m,
	MonadTrans (p x a), Monad (p x a m)) => TChan a -> p x a m ()
fromTChan c = lift (liftBase . atomically $ readTChan c) >>= yield >> fromTChan c

fromTChans :: (PipeClass p, MonadBase IO m,
	MonadTrans (p x a), Monad (p x a m)) => [TChan a] -> p x a m ()
fromTChans cs = do
	(cs', x) <- lift . liftBase . atomically $ do
		readTChans cs >>= maybe retry return
	yield x
	fromTChans $ uncurry (flip (++)) cs'

readTChans :: [TChan a] -> STM (Maybe (([TChan a], [TChan a]), a))
readTChans [] = return Nothing
readTChans (c : cs) = do
	e <- isEmptyTChan c
	if e
	then (first (first (c :)) <$>) <$>  readTChans cs
	else Just . (([c], cs) ,) <$> readTChan c

toTChan :: (PipeClass p, MonadBase IO m,
	MonadTrans (p a x), Monad (p a x m)) => TChan a -> p a x m ()
toTChan c = await >>= maybe (return ())
	((>> toTChan c) . lift . liftBase . atomically . writeTChan c)

toTChans :: (PipeClass p, MonadBase IO m,
	MonadTrans (p (a, b) x), Monad (p (a, b) x m)) =>
	[(a -> Bool, TChan b)] -> p (a, b) x m ()
toTChans cs = (await >>=) . maybe (return ()) $ \(t, x) -> (>> toTChans cs) $
	case find (($ t) . fst) cs of
		Just (_, c)-> lift . liftBase . atomically $ writeTChan c x
		_ -> return ()

toTChansM :: (PipeClass p, MonadBase IO m,
	MonadTrans (p (a, b) x), Monad (p (a, b) x m)) =>
	m [(a -> Bool, TChan b)] -> p (a, b) x m ()
toTChansM mcs = (await >>=) . maybe (return ()) $ \(t, x) -> (>> toTChansM mcs) $ do
	cs <- lift mcs
	case find (($ t) . fst) cs of
		Just (_, c)-> lift . liftBase . atomically $ writeTChan c x
		_ -> return ()
