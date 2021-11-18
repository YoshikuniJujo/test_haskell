{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Overlappable where

class IsChar c where isChar :: c -> String

instance IsChar Char where isChar _ = "It's Char!"

instance {-# OVERLAPPABLE #-} IsChar a where isChar _ = "It isn't Char."

class SayIsChar c where sayIsChar :: c -> IO ()

instance IsChar c => SayIsChar c where sayIsChar c = putStrLn $ isChar c

sayIsChar2 :: IsChar c => c -> IO ()
sayIsChar2 c = putStrLn $ isChar c
