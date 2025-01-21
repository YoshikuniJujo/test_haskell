{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.SwizzleSetClass2 where

import GHC.Generics
import Language.Haskell.TH hiding (Type)
import Data.Kind

import Template.Tools

import Try.SwizzleSetClass2.TH

(: []) <$> classGswizzle 1
(: []) <$> instanceGswizzle1K1
(: []) <$> instanceGswizzleM1 1
(: []) <$> instanceGswizzleProd 1

(: []) <$> classGswizzle 2
(: []) <$> instanceGswizzleM1 2
(: []) <$> instanceGswizzleProd 2

(: []) <$> tff
(: []) <$> instanceGswizzleProdProd 1
(: []) <$> instanceGswizzleProdProd 2

class SwizzleSet1 s b where
	type X s b
	x :: s -> b -> X s b

	default x :: (
		Generic s, Generic (X s b),
		GSwizzleSet1 (Rep s) b,
		Rep (X s b) ~ GX (Rep s) b ) =>
		s -> b -> X s b
	x s b = to (gx (from s) b)

instance SwizzleSet1 (a, b, c) d where type X (a, b, c) d = (d, b, c)
