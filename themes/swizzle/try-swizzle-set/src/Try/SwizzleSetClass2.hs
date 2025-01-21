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

(: []) <$> classSwizzleClass 1
(: []) <$> classSwizzleClass 2

instance SwizzleSet1 (a, b, c) d where type X (a, b, c) d = (d, b, c)
instance SwizzleSet2 (a, b, c) d where type Y (a, b, c) d = (a, d, c)
