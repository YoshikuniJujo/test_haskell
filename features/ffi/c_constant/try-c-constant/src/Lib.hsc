{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Int
import Text.Read
-- import Text.ParserCombinators.ReadPrec

import Template

#include "foo.h"

(: []) <$> mkNewtype "Foo" ''#{type Foo}

mkMembers "Foo" [
	("FooError", #{const FOO_ERROR}),
	("FooZero", #{const FOO_ZERO}),
	("FooOne", #{const FOO_ONE}),
	("FooTwo", #{const FOO_TWO}),
	("FooThree", #{const FOO_THREE}) ]

(: []) <$> mkShow "Foo" [
	"FooError",
	"FooZero",
	"FooOne",
	"FooTwo",
	"FooThree" ]

instance Read Foo where
	readPrec = parens $ choice [
		do Ident "FooError" <- lexP; pure FooError,
		prec 10 do Ident "Foo" <- lexP; Foo <$> step readPrec ]
