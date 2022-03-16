{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Exception where

import Control.Exception.Hierarchy

import qualified Data.ByteString as BS

import Shaderc.Exception.Enum
import Shaderc.CompilationResult.Core

data E = E CompilationStatus BS.ByteString deriving Show

exceptionHierarchy Nothing (ExType ''E)
