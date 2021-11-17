{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified Vulkan as Vk

main :: IO ()
main = run

run :: IO ()
run = do
	initVulkan
	mainLoop
	cleanup

initVulkan :: IO ()
initVulkan = pure ()

mainLoop :: IO ()
mainLoop = pure ()

cleanup :: IO ()
cleanup = pure ()
