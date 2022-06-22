{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeInfixOf where

import Data.Kind

class InfixIndex (ts :: [Type]) (ts' :: [Type]) where infixIndex :: Int

instance InfixIndex '[t] (t ': ts) where infixIndex = 0

instance {-# OVERLAPPABLE #-} InfixIndex ts ts' =>
	InfixIndex (t ': ts) (t ': ts') where
	infixIndex = 0

instance {-# OVERLAPPABLE #-} InfixIndex ts ts' =>
	InfixIndex ts (t ': ts') where
	infixIndex = 1 + infixIndex @ts @ts'
