{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.Generic.TH (instanceTuples) where

import Language.Haskell.TH
import Foreign.Storable
import Foreign.Storable.Generic.Internal

instanceTuples :: Int -> DecQ
instanceTuples n = bar =<< newName `mapM` take n vars

bar :: [Name] -> DecQ
bar vs = instanceD
	(cxt $ (conT ''Storable `appT`) . varT <$> vs)

	(conT ''G `appT` (foldl ((. varT) . appT) (tupleT $ length vs) vs))
	[]

vars :: [String]
vars = ((: "")
	<$> ['a' .. 'z']) ++ [ cs ++ [c] | cs <- vars, c <- ['a' .. 'z'] ]
