{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative

class Monad m => ErrorMonad m where
	throw :: String -> m a
	catch :: m a -> (String -> m a) -> m a

handle :: ErrorMonad m => (String -> m a) -> m a -> m a
handle = flip catch

instance ErrorMonad (Either String) where
	throw = Left
	catch (Left s) handler = handler s
	catch r _ = r

safeDiv :: Int -> Int -> Either String Int
_ `safeDiv` 0 = throw "can't divide by 0"
x `safeDiv` y = return $ x `div` y

divMsg :: Int -> Int -> Either String String
divMsg x y = handle (return . ("*** Error: " ++)) $ do
	d <- x `safeDiv` y
	return $ "The answer is " ++ show d
