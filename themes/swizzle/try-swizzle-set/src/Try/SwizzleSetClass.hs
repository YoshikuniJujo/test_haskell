{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.SwizzleSetClass where

import Try.SwizzleSetClass.TH

(: []) <$> classGswizzle 1
(: []) <$> instanceGswizzle1K1
(: []) <$> instanceGswizzleM1 1
(: []) <$> instanceGswizzleProd 1
(: []) <$> instanceGswizzleProdProd 1
(: []) <$> classSwizzleClass 1

(: []) <$> classGswizzle 2
(: []) <$> instanceGswizzleM1 2
(: []) <$> instanceGswizzleProd 2
(: []) <$> instanceGswizzleProdProd 2
(: []) <$> classSwizzleClass 2

(: []) <$> classGswizzle 3
(: []) <$> instanceGswizzleM1 3
(: []) <$> instanceGswizzleProd 3
(: []) <$> instanceGswizzleProdProd 3
(: []) <$> classSwizzleClass 3

instanceSwizzleTuple 1
instanceSwizzleTuple 2
instanceSwizzleTuple 3

xyz :: forall s t u v . (SwizzleSet1 u v, SwizzleSet2 t u, SwizzleSet3 s t) =>
	s -> (X v, Y u, Z t) -> v
xyz s (a, b, c) = flip (x @u) a . flip (y @t) b $ flip z c s
