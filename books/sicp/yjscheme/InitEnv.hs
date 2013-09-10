{-# LANGUAGE PackageImports #-}

module InitEnv (
	prs, dpt, eval,

	SchemeM, runEnvT, runSchemeM,
	initEnv,
	Object(OError), showObj, showObjM,
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
	("quote", OSyntax "quote" quote),
	("let", OSyntax "let" lets),
	("error", OSubr "error" errors),
	("sin", OSubr "sin" sins),
	("cos", OSubr "cos" coss),
	("log", OSubr "log" logs),
	("exp", OSubr "exp" exps),
	("cons", OSubr "cons" conss),
	("car", OSubr "car" cars),
	("cdr", OSubr "cdr" cdrs),
	("list", OSubr "list" list),
	("null?", OSubr "null?" nulls)
 ]

load :: Object -> SchemeM Object
load o = do
	l <- cons2list o
	case l of
		[OString fp] -> do
			src <- liftIO $ readFile fp
			case prsf src of
				Just os -> mapM_ eval os >> return (OBool True)
				_ -> throwError "*** ERROR: parse error"
		_ -> throwError "*** ERROR: load: bad arguments"
