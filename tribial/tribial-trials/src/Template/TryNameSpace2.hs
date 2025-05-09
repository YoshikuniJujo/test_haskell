{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.TryNameSpace2 where

import Template.NameSpace

foo :: Int
foo = $fooTh''

foo6 :: Int
foo6 = $fooTh6
