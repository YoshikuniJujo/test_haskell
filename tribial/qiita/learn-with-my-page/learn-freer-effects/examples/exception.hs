{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Freer

newtype Exc e a = Exc e

throwError :: e -> Freer (Exc e) a
throwError = freer . Exc

runError :: Freer (Exc e) a -> Either e a
runError = \case
	Pure x -> Right x
	Exc e `Bind` _k -> Left e

safeDiv :: Integer -> Integer -> Freer (Exc String) Integer
safeDiv n 0 = throwError $ show n ++ " is divided by 0"
safeDiv n m = return $ n `div` m

catchError :: Freer (Exc e) a -> (e -> Freer (Exc e) a) -> Freer (Exc e) a
m `catchError` h = case m of
	Pure x -> return x
	Bind (Exc e) _k -> h e

sample :: Integer -> Integer -> Freer (Exc String) Integer
sample n m = do
	a <- 50 `safeDiv` n
	b <- (100 `safeDiv` m) `catchError` const (return 50000)
	return $ a + b
