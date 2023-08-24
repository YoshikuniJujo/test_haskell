{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.Trans
import Options.Declarative

main :: IO ()
main = run_ greet

greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String) ->
	Flag "l" '["list"] "[STRING]" "list" [String] ->
	Arg "NAME" String ->
	Arg "LIST" [String] ->
	Cmd "Greeting command" ()
greet msg flst name lst = liftIO do
	putStrLn $ get msg ++ ", " ++ get name ++ "!"
	print $ get flst
	print $ get lst
