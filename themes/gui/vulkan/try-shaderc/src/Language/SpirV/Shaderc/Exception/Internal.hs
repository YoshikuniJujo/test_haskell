{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Language.SpirV.Shaderc.Exception.Internal where

import Control.Exception
import Control.Exception.Hierarchy

import qualified Data.ByteString as BS

import Language.SpirV.Shaderc.Exception.Enum

import qualified Shaderc.CompilationResult.Core as CompilationResult

data E = E CompilationStatus BS.ByteString deriving Show

exceptionHierarchy Nothing (ExType ''E)

throwUnlessSuccess :: CompilationResult.T -> IO ()
throwUnlessSuccess rslt = do
	stt <- CompilationResult.getCompilationStatus rslt
	case stt of
		CompilationStatusSuccess -> pure ()
		_ -> do	cmsg <- CompilationResult.getErrorMessage rslt
			msg <- BS.packCString cmsg
			throw $ E stt msg
