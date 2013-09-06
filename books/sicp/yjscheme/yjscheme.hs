{-# LANGUAGE PackageImports #-}

module Main where

import System.IO
import System.Exit
import Control.Monad
import "monads-tf" Control.Monad.Trans

import Parser
import Eval

main :: IO ()
main = runEnvT testEnv $ forever $ do
	ln <- prompt
	if ln == "(exit)" then liftIO $ exitWith ExitSuccess else return ()
	flip catchError (liftIO . putStrLn) $ do
		ret <- case prs ln of
			Just obj -> eval obj
			_ -> throwError "*** READ-ERROR:\n"
		liftIO $ putStrLn $ showObj ret

prompt :: EnvT Object IO String
prompt = liftIO $ do
	putStr "yjscheme> "
	hFlush stdout
	getLine
