{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module DeriveGenericTuple where

import DeriveGenericTupleGen

concat <$> deriveGenericTuple `mapM` [2 .. 62]
