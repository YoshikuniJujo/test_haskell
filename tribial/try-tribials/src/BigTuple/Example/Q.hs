{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BigTuple.Example.Q where

import Language.Haskell.TH

data Tuple3 a b c = Tuple3 a b c deriving Show

foo0 :: DecsQ
foo0 = [d| data Tuple3 a b c = Tuple3 a b c |]

foo :: ExpQ
foo = [| Tuple3 1 2 3 |]

bar :: TypeQ
bar = [t| Tuple3 Int Char () |]

baz :: PatQ
baz = [p| Tuple3 x y z |]
