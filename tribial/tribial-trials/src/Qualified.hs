{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Qualified where

import Qualified.A as A

import Qualified.B qualified

import Qualified.C

import Qualified.D qualified as D

foo, bar :: Int
foo = A.eight
bar = eight
-- baz = Qualified.A.eight

qux :: Int
qux = Qualified.B.three
-- quux = three

corge, grault :: Int
corge = two
grault = Qualified.C.two

garply :: Int
garply = D.twelve
-- waldo = Qualified.D.twelve
