{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Hsc2hs where

#include <foo.h>

foo :: Int
foo = #{const Foo}

hello :: String
hello = #{const_str hello}
