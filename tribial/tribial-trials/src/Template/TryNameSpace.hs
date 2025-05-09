{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-unused-imports #-}

module Template.TryNameSpace where

import qualified Template.NameSpace as Ns

import qualified Template.Foo as F

foo :: Int
foo = 123

bar :: Int
bar = $(Ns.fooTh)

-- bar' :: Int
-- bar' = $(Ns.fooTh')

bar'' :: Int
bar'' = $(Ns.fooTh'')

bar''' :: Int
bar''' = $(Ns.fooTh''')

bar4 :: Int
bar4 = $(Ns.fooTh4)
