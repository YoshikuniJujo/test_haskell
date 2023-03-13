{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeFamilies.TestAssociated where

import Data.Char

class Foo a b where
	type Bar a

instance Foo Int Double where
	type Bar Int = Char

instance Foo Int Word where
--	type Bar Int = ()
	type Bar Int = Char

foo :: Bar Int -> Char
foo = toUpper
