{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import GHC.Generics

import TryTh

do	runIO . print =<< reifyInstances ''Generic [ConT ''(,) `AppT` VarT (mkName "a") `AppT` VarT (mkName "b")]
	runIO . print =<< isInstance ''Generic [ConT ''(,) `AppT` VarT (mkName "a") `AppT` VarT (mkName "b")]
	runIO . print =<< isInstance ''Generic [tupTN 7]
	runIO . print =<< isInstance ''Generic [tupTN 8]
	pure []

main :: IO ()
main = putStrLn "Slozsoft"
