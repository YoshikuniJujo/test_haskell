{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.Trans
import Options.Declarative

main :: IO ()
main = run_ showOpts

showOpts ::
	Flag "c" '["config"]
		"FILEPATH" "Configuration File" (Def "defaultFishParams" String)
	-> Flag "o" '["output"] "FILEPATH" "Result .png File" String
	-> Cmd "Filled with fishes" ()
showOpts cnf op = liftIO do
	putStrLn $ get cnf
	putStrLn $ get op
