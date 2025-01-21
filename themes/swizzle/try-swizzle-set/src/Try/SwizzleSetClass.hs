{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
