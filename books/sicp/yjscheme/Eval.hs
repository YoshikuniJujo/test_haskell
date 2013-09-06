{-# LANGUAGE PackageImports #-}

module Eval (
	eval,
	runEnvT, testEnv, nullEnv, EnvT, Object,
	throwError, catchError,
) where

import Object
import Env

import Control.Applicative

type SchemeM = EnvT Object IO

eval :: Object -> SchemeM Object
eval i@(OInt _) = return i
eval (OVar v) = getValue v
eval (OCons f_ as_) = do
	f <- eval f_
	case f of
		OSubr _ s -> do
			er <- s <$> mapCons eval as_
			case er of
				Right r -> return r
				Left err -> throwError err
		_ -> error "eval: bad"
eval ONil = return ONil
eval s@(OSubr _ _) = return s

mapCons :: (Object -> SchemeM Object) -> Object -> SchemeM Object
mapCons _ ONil = return ONil
mapCons f (OCons a d) = OCons <$> f a <*> mapCons f d
mapCons _ o = throwError $
	"*** ERROR: proper list required for function application or macro use: "
	++ showObj o

foldlCons :: (Object -> Object -> Either String Object) -> Object -> Object ->
	Either String Object
foldlCons _ o0 ONil = Right o0
foldlCons f o0 (OCons a d) = case f o0 a of
	Right r -> foldlCons f r d
	err -> err
foldlCons _ _ _ = Left "foldlCons: bad"

testEnv :: Env Object
testEnv = [
	("+", OSubr "+" $ foldlCons add (OInt 0)),
	("-", OSubr "-" subAll),
	("*", OSubr "*" $ foldlCons mul (OInt 1))
 ]

add :: Object -> Object -> Either String Object
add (OInt i1) (OInt i2) = Right $ OInt $ i1 + i2
add x y = Left $ "*** ERROR: operation + is not defined between " ++
	showObj x ++ " and " ++ showObj y

mul :: Object -> Object -> Either String Object
mul (OInt i1) (OInt i2) = Right $ OInt $ i1 * i2
mul x y = Left $ "*** ERROR: operation * is not defined between " ++
	showObj x ++ " and " ++ showObj y

subAll :: Object -> Either String Object
subAll (OCons (OInt i) ONil) = Right $ OInt $ - i
subAll (OCons o ONil) =
	Left $ "*** ERROR: operation - is not defined on object " ++ showObj o
subAll (OCons i0 is) = foldlCons sub i0 is
subAll ONil = Left $ "*** ERROR: procedure requires at least one argument: (-)"
subAll o = Left $
	"*** ERROR: proper list required for function application or macro use: " ++
	showObj o

sub :: Object -> Object -> Either String Object
sub (OInt i1) (OInt i2) = Right $ OInt $ i1 - i2
sub x y = Left $ "*** ERROR: operation - is not defined between " ++
	showObj x ++ " and " ++ showObj y

-- div' :: Object -> Object -> Either String Object
-- div' (OInt i1) (OInt i2) = 
