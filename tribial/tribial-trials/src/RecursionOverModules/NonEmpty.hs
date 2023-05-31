{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RecursionOverModules.NonEmpty where

import {-# SOURCE #-} RecursionOverModules.List qualified as L

data NonEmpty a = a :. L.List a deriving Show

len :: NonEmpty a -> Int
len (_ :. l) = 1 + L.len l
