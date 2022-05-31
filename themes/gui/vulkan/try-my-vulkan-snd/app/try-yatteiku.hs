{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Vulkan.Base

import qualified Vulkan.Instance as Vk.Instance

main :: IO ()
main = Vk.Instance.create Vk.Instance.createInfoNil nil nil \inst ->
	pure ()
