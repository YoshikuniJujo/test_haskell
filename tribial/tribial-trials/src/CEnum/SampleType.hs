{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CEnum.SampleType where

import Language.Haskell.TH
import Data.Proxy

import CEnum.SampleType.Th

import qualified CEnum.Sample as E

do	is <- lines <$> runIO (readFile "th/enumSample.txt")
	(: []) <$> dataD (pure []) (mkName "EnumSample") [] Nothing
		((`normalC` []) . mkName <$> is)
		[derivClause Nothing [conT ''Show]]

class EnumSampleToValue (t :: EnumSample) where
	enumSampleToValue :: E.EnumSample

do	is <- lines <$> runIO (readFile "th/enumSample.txt")
	sequence $ mkInstance <$> is

do
	is <- lines <$> runIO (readFile "th/enumSample.txt")
	sequence [
		sigFoo,
		funD (mkName "enumSampleToType") (foo <$> is) ]

enumSampleToType' :: E.EnumSample ->
	(forall (t :: EnumSample) . EnumSampleToValue t => Proxy t -> a) -> a
enumSampleToType' E.EnumSample1 f = f (Proxy @'EnumSample1)
enumSampleToType' _ _ = undefined
