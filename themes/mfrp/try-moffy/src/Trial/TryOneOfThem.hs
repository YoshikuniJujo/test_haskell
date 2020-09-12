{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Trial.TryOneOfThem where

import Data.Type.Set
import Data.OneOfThem

numbered [t| Bool |]
numbered [t| () |]
numbered [t| Char |]

foo, bar, baz :: OneOfThem (Bool :- () :- Char :- 'Nil)
foo = expand $ Singleton True
bar = expand $ Singleton ()
baz = expand $ Singleton 'c'
