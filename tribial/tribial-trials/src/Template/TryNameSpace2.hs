{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.TryNameSpace2 where

import Template.NameSpace

foo = $fooTh''

foo6 = $fooTh6
