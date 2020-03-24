{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Numbered (Numbered, numbered) where

import GHC.TypeLits
import Language.Haskell.TH
import System.Random

import qualified Data.Kind as K

class Numbered a where
	type Number (a :: K.Type) = (r :: Nat) | r -> a

(: []) <$> instanceD (cxt []) (conT ''Numbered `appT` conT ''Int) [
	tySynInstD $ tySynEqn Nothing (conT ''Number `appT` conT ''Int) (litT $ numTyLit 108)
	]

numbered :: TypeQ -> DecsQ
numbered t = ((: []) <$>) . instanceD (cxt []) (conT ''Numbered `appT` t) $ (: []) do
	n <- runIO $ abs <$> randomIO
	tySynInstD $ tySynEqn Nothing (conT ''Number `appT` t) (litT $ numTyLit n)
