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
	(\a b c -> a ++ b ++ c)	
		<$> ((: []) <$> mkType "EnumSample" is)
		<*> ((:)	<$> mkClass "E" "EnumSample"
				<*> sequence (mkInstance "E" "EnumSample" <$> is))
		<*> sequence [
			sigFoo "E" "EnumSample",
			funD (mkName "enumSampleToType") (foo <$> is) ]
