{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad.IO.Class
import Options.Declarative

main :: IO ()
main = run_ realMain

realMain :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String) ->
	Cmd "Greeting command" ()
realMain msg = liftIO $ putStrLn (get msg)
