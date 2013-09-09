{-# LANGUAGE PackageImports #-}

module InitEnv (
	prs, dpt, eval,

	SchemeM, runEnvT, runSchemeM,
	initEnv,
	Object(OError), showObj,
	throwError, catchError,
) where

import Parser
import Subrs
import "monads-tf" Control.Monad.Trans

initEnv :: Environment Object
initEnv = mkInitEnv [
	("+", OSubr "+" $ foldlCons add (OInt 0)),
	("-", OSubr "-" subAll),
	("*", OSubr "*" $ foldlCons mul (OInt 1)),
	("/", OSubr "/" divAll),
	(">", OSubr ">" $ bopSeq ">" (>)),
	("<", OSubr "<" $ bopSeq "<" (<)),
	("=", OSubr "=" $ bopSeq "=" (==)),
	("quotient", OSubr "quotient" quotient),
	("remainder", OSubr "remainder" remainder),
	("expt", OSubr "expt" expt),
	("logbit?", OSubr "logbit?" logbit),
	("random", OSubr "random" rndm),
	("exit", OSubr "exit" exit),
	("define", OSyntax "define" def),
	("lambda", OSyntax "lambda" $ lambda Nothing),
	("cond", OSyntax "cond" cond),
	("if", OSyntax "if" ifs),
	("and", OSyntax "and" ands),
	("or", OSyntax "or" ors),
	("not", OSubr "not" nots),
	("load", OSubr "load" load),
	("display", OSubr "display" display),
	("runtime", OSubr "runtime" runtime),
	("quote", OSyntax "quote" quote)
 ]

load :: Object -> SchemeM Object
load (OCons (OString fp) ONil) = do
	src <- liftIO $ readFile fp
	case prsf src of
		Just os -> mapM_ eval os >> return (OBool True)
		_ -> throwError "*** ERROR: parse error"
load _ = throwError "*** ERROR: load: bad arguments"
