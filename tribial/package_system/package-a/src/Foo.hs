{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo (module Foo) where

import "package-b" Foo
