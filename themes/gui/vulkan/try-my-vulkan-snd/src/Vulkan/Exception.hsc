{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Vulkan.Exception where

import Control.Exception
import Control.Exception.Hierarchy

import Vulkan.Exception.Enum

exceptionHierarchy Nothing (ExType ''Result)

throwUnlessSuccess :: Result -> IO ()
throwUnlessSuccess = \case Success -> pure (); e -> throw e
