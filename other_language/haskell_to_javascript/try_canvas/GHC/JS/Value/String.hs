{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.String where

import Prelude hiding (String)
import Prelude qualified

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value

data String = String JSVal

instance JS.Value.IsJSVal String where toJSVal (String v) = v

instance JS.Value.V String

toJS :: Prelude.String -> String
toJS = String . toJSString

fromJS :: String -> Prelude.String
fromJS (String s) = fromJSString s
