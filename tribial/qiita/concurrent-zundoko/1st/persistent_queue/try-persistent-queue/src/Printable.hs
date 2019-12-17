{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Printable where

class Printable a where show' :: a -> IO String
	
print' :: Printable a => a -> IO ()
print' x = putStrLn =<< show' x

instance {-# OVERLAPPABLE #-} Show a => Printable a where
	show' = return . show

instance Printable a => Printable (Maybe a) where
	show' (Just x) = ("Just (" ++) . (++ ")") <$> show' x
	show' Nothing = return "Nothing"

instance (Printable a, Printable b) => Printable (a, b) where
	show' (x, y) =
		(\s t -> "(" ++ s ++ "," ++ t ++ ")") <$> show' x <*> show' y
