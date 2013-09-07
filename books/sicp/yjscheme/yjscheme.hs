{-# LANGUAGE PackageImports #-}

module Main where

import InitEnv

import Data.Maybe
import Data.Char
import System.IO
import Control.Monad
import "monads-tf" Control.Monad.Trans

main :: IO ()
main = runEnvT initEnv $ do
	forever $ do
		ln <- prompt 0 ""
		flip catchError (liftIO . putStrLn) $ do
			ret <- case prs ln of
				Just obj -> eval obj
				_ -> throwError "*** READ-ERROR:\n"
			liftIO $ putStrLn $ showObj ret

prompt :: Int -> String -> SchemeM String
prompt d s = do
	n <- liftIO $ do
		putStr $ "yjscheme:" ++ show d ++ "> "
		hFlush stdout
		getLine
	let	s' = s ++ " " ++ n
		d' = dpt s'
	if maybe False (> 0) d' || all isSpace s'
		then prompt (fromJust d') s'
		else return s'
