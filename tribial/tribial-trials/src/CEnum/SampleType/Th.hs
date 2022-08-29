{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CEnum.SampleType.Th where

import Language.Haskell.TH

mkInstance :: String -> Q Dec
mkInstance ss =
	instanceD (pure []) (conT (mkName "EnumSampleToValue") `appT` promotedT (mkName ss)) [
		valD (varP $ mkName "enumSampleToValue") (normalB . conE . mkName $ "E." ++ ss) []
		]
