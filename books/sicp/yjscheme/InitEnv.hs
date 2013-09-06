module InitEnv (
	prs, dpt, eval,

	SchemeM, runEnvT,
	initEnv,
	Object, showObj,
	throwError, catchError,
) where

import Parser
import Subrs

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
	("cond", OSyntax "cond" cond)
 ]
