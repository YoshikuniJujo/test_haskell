{-# LANGUAGE PackageImports, RankNTypes #-}

module Subrs (
	Env, fromList,
	EnvT, runEnvT,
	Object(..),
	eval,
	throwError, catchError,

	foldlCons,
	add,
	subAll,
	mul,
	exit
) where

import Eval

import "monads-tf" Control.Monad.Trans
import System.Exit

add, mul, sub :: Object -> Object -> SchemeM Object
add = numOp "+" (+)
mul = numOp "*" (*)
sub = numOp "-" (-)

subAll :: Object -> SchemeM Object
subAll = oneOrMore "-" (numFun "-" negate) sub

exit :: Object -> SchemeM Object
exit ONil = liftIO exitSuccess >> return OUndef
exit (OCons (OInt ec) ONil)
	| ec == 0 = exit ONil
	| otherwise = liftIO (exitWith $ ExitFailure $ fromIntegral ec) >>
		return OUndef
exit _ = throwError "*** ERROR: bad arguments"

numFun :: String -> (forall a . Num a => a -> a) -> Object -> SchemeM Object
numFun _ f (OInt i) = return $ OInt $ f i
numFun n _ x = throwError $
	"*** ERROR: operation " ++ n ++ " is not defined on object " ++ showObj x

numOp :: String -> (forall a . Num a => a -> a -> a) -> Object -> Object ->
	SchemeM Object
numOp _ op (OInt i1) (OInt i2) = return $ OInt $ i1 `op` i2
numOp n _ x y = throwError $
	"*** ERROR: operation " ++ n ++ " is not defined between " ++
	showObj x ++ " and " ++ showObj y

oneOrMore :: String -> (Object -> SchemeM Object) ->
	(Object -> Object -> SchemeM Object) -> Object -> SchemeM Object
oneOrMore _ f _ (OCons o ONil) = f o
oneOrMore _ _ op (OCons o0 os) = foldlCons op o0 os
oneOrMore n _ _ ONil = throwError $
	"*** ERROR: procedure requires at least one argument: (" ++ n ++ ")"
oneOrMore _ _ _ o = throwError $
	"*** ERROR: proper list required for function application or macro use: " ++
	showObj o

-- div' :: Object -> Object -> Either String Object
-- div' (OInt i1) (OInt i2) = 
