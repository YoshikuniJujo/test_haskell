{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.Trans
import Options.Declarative

main :: IO ()
main = run_ greet

greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String) ->
	Arg "NAME" String ->
	Cmd "Greeting command" ()
greet msg name = liftIO . putStrLn $ get msg ++ ", " ++ get name ++ "!"
