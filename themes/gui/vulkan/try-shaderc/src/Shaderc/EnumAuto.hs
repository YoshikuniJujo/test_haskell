{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.EnumAuto where

import Language.Haskell.TH
import Data.Traversable

import qualified Data.ByteString as BS

import qualified Shaderc.EnumAuto.Core as C

(: []) <$> do
	enms <- runIO $ lines <$> readFile "data/ShaderKind.txt"
	dataD (cxt []) (mkName "ShaderKind") [] Nothing
		((\enm -> normalC (mkName enm) []) <$> enms)
		[derivClause Nothing [conT ''Show]]

data Spv (sknd :: ShaderKind) = Spv BS.ByteString

class SpvShaderKind (sknd :: ShaderKind) where
	shaderKind :: C.ShaderKind

do	enms <- runIO $ lines <$> readFile "data/ShaderKind.txt"
	pairs <- for enms \nm -> do
		Just foo <- lookupValueName nm
		Just bar <- lookupValueName $ "C." ++ nm
		pure (foo, bar)
	(\(knd, cknd) -> instanceD (cxt []) (conT ''SpvShaderKind `appT` promotedT knd)
		[valD (varP 'shaderKind) (normalB $ conE cknd) []]) `mapM` pairs
