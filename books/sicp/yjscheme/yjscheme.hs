{-# LANGUAGE PackageImports, RankNTypes, TupleSections #-}

module Main where

import Data.Maybe
import System.IO
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error

import Parser
import Eval
import InitEnv

main :: IO ()
main = do
	_ <- runErrorT $ flip runStateT ((topEID, Nothing), ie) $ do
		doWhile_ $ do
			ln <- prompt 0 ""
			case prs ln of
				Just p -> printObj $ eval p
				Nothing -> liftIO $ putStrLn $ "parse error: " ++ ln
			return True
	return ()
	where
	doWhile_ act = do
		b <- act
		if b then doWhile_ act else return ()
	ie = (EVar "load", OSubr "load" load) : initEnv

prompt :: Int -> String -> SchemeM String
prompt d s = do
	n <- liftIO $ do
		putStr $ "yjscheme:" ++ show d ++ "> "
		hFlush stdout
		getLine
	let	s' = s ++ " " ++ n
		d' = dpt s'
	if maybe False (> 0) d'
		then prompt (fromJust d') s'
		else return s'

printObj :: SchemeM Object -> SchemeM ()
printObj o = catchError (o >>= liftIO . putStrLn . showObj) $ \e ->
	liftIO (putStrLn e)

load :: Object -> SchemeM Object
load (OCons (OString fp) ONil) = do
	src <- liftIO $ readFile fp
	case prsf src of
		Just os -> mapM_ eval os >> return (OBool True)
		_ -> throwError "*** ERROR: parse error"
load _ = throwError "*** ERROR: load: bad arguments"
