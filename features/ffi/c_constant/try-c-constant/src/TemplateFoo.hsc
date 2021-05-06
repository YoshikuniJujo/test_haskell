{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TemplateFoo where

import Language.Haskell.TH
-- import Data.Int

-- import Template

#include "foo.h"

-- newtype Foo = Foo #{type Foo} deriving Show

-- mkMemberFoo :: String -> Integer -> DecsQ
-- mkMemberFoo = mkMemberGen ''Foo 'Foo
