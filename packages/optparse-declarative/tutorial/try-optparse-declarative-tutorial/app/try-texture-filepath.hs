{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.Trans
import Options.Declarative

greet :: Flag "t" '["texture"] "FILEPATH" "texture filepath" String ->
	Cmd "Try Vulkan Texture" ()
greet msg = liftIO . putStrLn $ "texture: " ++ get msg

main :: IO ()
main = run_ greet
