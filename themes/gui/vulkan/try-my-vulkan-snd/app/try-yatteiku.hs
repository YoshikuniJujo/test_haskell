{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Vulkan.Base

import qualified Vulkan.Instance.Middle as Vk.Instance

main :: IO ()
main = do
	inst <- Vk.Instance.create Vk.Instance.createInfoNil nil
	Vk.Instance.destroy inst nil
