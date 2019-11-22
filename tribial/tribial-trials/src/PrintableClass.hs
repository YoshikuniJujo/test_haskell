{-# LANGUAGE DefaultSignatures, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PrintableClass where

class Printable a where
	print' :: a -> IO ()
	default print' :: Show a => a -> IO ()
	print' = putStrLn . show

instance Printable Integer where
	print' x = putStrLn $ "check overlap: " ++ show x

instance {-# Overlappable #-} Show a => Printable a
