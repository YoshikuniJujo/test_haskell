{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ContinuationMonad where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Control.Monad.Trans
import Control.Monad.Cont

alloca2 :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO c) -> IO c
alloca2 = (. uncurry) . runContT $ (,) <$> ContT alloca <*> ContT alloca

withCStringList :: [String] -> ([CString] -> IO a) -> IO a
withCStringList = runContT . ((ContT . withCString) `mapM`)

foo :: IO ()
foo = alloca \p ->
	alloca \q ->
		alloca \r -> do
			poke p 'c'
			poke q False
			poke r ()
			print =<< peek p
			print =<< peek q
			print =<< peek r

bar :: IO ()
bar = runContT ((,,)
	<$> ContT alloca <*> ContT alloca <*> ContT alloca) \(p, q, r) -> do
		poke p 'c'
		poke q False
		poke r ()
		print =<< peek p
		print =<< peek q
		print =<< peek r

baz :: IO ()
baz = ($ pure) $ runContT do
	p <- ContT alloca
	q <- ContT alloca
	r <- ContT alloca
	lift $ do
		poke p 'c'
		poke q False
		poke r ()
		print =<< peek p
		print =<< peek q
		print =<< peek r
