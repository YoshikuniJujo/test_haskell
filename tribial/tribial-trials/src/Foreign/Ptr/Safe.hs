{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Ptr.Safe (
	S, safeAlloca, safePeek, safePoke, SafeContT(..),
	sample0
	) where

import Control.Monad
import Control.Monad.Cont
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Kind

newtype S a s = S (Ptr a) deriving Show

safeAlloca :: Storable a => (forall s . S a s -> IO b) -> IO b
safeAlloca f = alloca $ f . S

safeAllocaList :: Storable a => Int -> (forall s . [S a s] -> IO b) -> IO b
safeAllocaList n f = runContT (replicateM n $ ContT alloca) $ f . (S <$>)

safePeek :: Storable a => S a s -> IO a
safePeek (S p) = peek p

safePoke :: Storable a => S a s -> a -> IO ()
safePoke (S p) = poke p

newtype SafeContT (r :: k) (m :: k -> Type) a = SafeContT {
	runSafeContT :: (forall s . a s -> m r) -> m r }

newtype SInt s = SInt Int deriving Show

sample0 :: IO (Int, Char)
sample0 =
	safeAlloca \a ->
	safeAlloca \b -> do
		safePoke a 123
		safePoke b 'c'
		(,) <$> safePeek a <*> safePeek b

type SafeM r m a = (forall s . a s -> m r) -> m r
type SafeListM r m a = (forall s . [a s] -> m r) -> m r

{-
replicateSafe :: Int -> SafeM r m a -> SafeListM r m a
replicateSafe n _ f | n < 1 = f []
replicateSafe n m f = m \x -> replicateSafe (n - 1) m \xs -> f (x : xs)
-}

{-
-- sampleSafeContT :: SafeContT r IO (Safe (Int, Char))
sampleSafeContT :: SafeContT r IO SInt
sampleSafeContT = do
	a <- SafeContT $ safeAlloca
	b <- SafeContT $ safeAlloca
	lift $ safePoke a 123
	lift $ safePoke b 'c'
--	(,) <$> lift (safePeek a) <*> lift (safePeek b)
	SInt <$> lift (safePeek a)
	-}
