{-# LANGUAGE DefaultSignatures, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

class Printable a where
	print' :: a -> IO ()
	default print' :: Show a => a -> IO ()
	print' = putStrLn . show

instance {-# OVERLAPPABLE #-} Show a => Printable a

instance Printable Integer where
	print' = putStrLn . (++ " :: Integer") . show
