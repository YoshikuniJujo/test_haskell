{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Examples where

import qualified FreerNoFtcq as Nf
import qualified Freer as F

newtype Exc e a = Exc e

throwErrorNf :: e -> Nf.Freer (Exc e) a
throwErrorNf = Nf.freer . Exc

runErrorNf :: Nf.Freer (Exc e) a -> Either e a
runErrorNf = \case
	Nf.Pure x -> Right x
	Exc e `Nf.Bind` _k -> Left e

safeDivNf :: Integer -> Integer -> Nf.Freer (Exc String) Integer
safeDivNf n 0 = throwErrorNf $ show n ++ " is divided by 0"
safeDivNf n m = return $ n `div` m

sampleNf :: Integer -> Nf.Freer (Exc String) Integer
sampleNf n = do
	x <- 30 `safeDivNf` 6
	y <- 45 `safeDivNf` n
	undefined
	return (x + y)
