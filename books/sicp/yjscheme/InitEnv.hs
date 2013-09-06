module InitEnv (
	prs, dpt, eval,

	SchemeM, runEnvT,
	initEnv,
	Object, showObj,
	throwError, catchError,
) where

import Parser
import Subrs

initEnv :: Env Object
initEnv = fromList [
	("+", OSubr "+" $ foldlCons add (OInt 0)),
	("-", OSubr "-" subAll),
	("*", OSubr "*" $ foldlCons mul (OInt 1)),
	("/", OSubr "/" divAll),
	("exit", OSubr "exit" exit)
 ]
