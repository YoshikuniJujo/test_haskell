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

typeValues "E" "EnumSample" =<< lines <$> runIO (readFile "th/enumSample.txt")
