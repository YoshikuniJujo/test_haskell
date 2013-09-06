{-# LANGUAGE PackageImports #-}

module InitEnv (
	prs, dpt, eval,

	SchemeM, runEnvT,
	initEnv,
	Object, showObj,
	throwError, catchError,
) where

import Parser
import Subrs
import "monads-tf" Control.Monad.Trans

initEnv :: Environment Object
initEnv = fromList [
	("+", OSubr "+" $ foldlCons add (OInt 0)),
	("-", OSubr "-" subAll),
	("*", OSubr "*" $ foldlCons mul (OInt 1)),
	("/", OSubr "/" divAll),
	(">", OSubr ">" $ bopSeq ">" (>)),
	("<", OSubr "<" $ bopSeq "<" (<)),
	("=", OSubr "=" $ bopSeq "=" (==)),
	("exit", OSubr "exit" exit),
	("define", OSyntax "define" def),
	("lambda", OSyntax "lambda" $ lambda Nothing),
	("cond", OSyntax "cond" cond),
	("if", OSyntax "if" ifs),
	("load", OSubr "load" load)
 ]

load :: Object -> SchemeM Object
load (OCons (OString fp) ONil) = do
	src <- liftIO $ readFile fp
	case prsf src of
		Just os -> mapM_ eval os >> return (OBool True)
		_ -> throwError "*** ERROR: parse error"
load _ = throwError "*** ERROR: load: bad arguments"
