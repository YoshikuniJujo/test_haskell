{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuDepthStencilCompare where

import GHC.JS.Value qualified as JS.Value

data G	= Never | Always | Equal | NotEqual
	| Less | LessEqual | Greater | GreaterEqual
	deriving Show

toString :: G -> String
toString = \case
	Never -> "never"; Always -> "always"
	Equal -> "equal"; NotEqual -> "not-equal"
	Less -> "less"; LessEqual -> "less-equal"
	Greater -> "greater"; GreaterEqual -> "greater-equal"

instance JS.Value.IsJSVal G where toJSVal = JS.Value.toJSVal . toString
instance JS.Value.V G
