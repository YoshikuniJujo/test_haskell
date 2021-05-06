{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Int

import Template

#include "foo.h"

(: []) <$> mkNewtype "Foo" ''#{type Foo}

mkMemberGen ''Foo 'Foo "FooError" (#{const FOO_ERROR})
mkMemberGen ''Foo 'Foo "FooZero" #{const FOO_ZERO}
mkMemberGen ''Foo 'Foo "FooOne" #{const FOO_ONE}
mkMemberGen ''Foo 'Foo  "FooTwo" #{const FOO_TWO}
mkMemberGen ''Foo 'Foo  "FooThree" #{const FOO_THREE}
