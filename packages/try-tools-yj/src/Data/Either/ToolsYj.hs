{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Either.ToolsYj (forceRight, forceRight') where

import Control.Exception

forceRight :: Exception e => Either e a -> a
forceRight = \case Left e -> throw e; Right x -> x

forceRight' :: Either String a -> a
forceRight' = forceRight . either (Left . ErrorCall) Right
