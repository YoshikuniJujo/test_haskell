{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.Trans
import Options.Declarative

greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String) ->
	Arg "NAME" String -> Cmd "Simple greeting example" ()
greet msg name = liftIO . putStrLn $ get msg ++ ", " ++ get name ++ "!"

main :: IO ()
main = run_ greet
