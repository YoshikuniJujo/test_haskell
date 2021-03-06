{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Int

import Foreign.C.Enum

#include "foo.h"

enum "Foo" ''#{type Foo} [''Show, ''Read, ''Eq] [
	("FooError", #{const FOO_ERROR}),
	("FooZero", #{const FOO_ZERO}),
	("FooOne", #{const FOO_ONE}),
	("FooTwo", #{const FOO_TWO}),
	("FooThree", #{const FOO_THREE}) ]
