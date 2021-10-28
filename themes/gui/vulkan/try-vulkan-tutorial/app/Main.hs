{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Bool

import Lib

main :: IO ()
main = do
	w <- initWindow
	initVulkan
	mainLoop w
	cleanup w

initWindow :: IO GlfwWindow
initWindow = do
	glfwInit
	glfwWindowHint GlfwClientApi GlfwNoApi
	glfwWindowHint GlfwResizable GlfwFalse
	glfwCreateWindowSimple 800 600 "Vulkan"

initVulkan :: IO ()
initVulkan = pure ()

mainLoop :: GlfwWindow -> IO ()
mainLoop w = do
	cls <- glfwWindowShouldClose w
	glfwPollEvents
	bool (mainLoop w) (pure ()) cls

cleanup :: GlfwWindow -> IO ()
cleanup w = do
	glfwDestroyWindow w
	glfwTerminate
