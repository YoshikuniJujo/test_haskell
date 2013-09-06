{-# LANGUAGE PackageImports #-}

module Eval (
	eval,
	runEnvT, testEnv, nullEnv, EnvT, Object,
	throwError, catchError,
) where

import Object

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import System.Exit

eval :: Object -> SchemeM Object
eval i@(OInt _) = return i
eval (OVar v) = getValue v
eval (OCons f_ as_) = do
	f <- eval f_
	case f of
		OSubr _ s -> s =<< mapCons eval as_
		_ -> error "eval: bad"
eval ONil = return ONil
eval s@(OSubr _ _) = return s
eval u@OUndef = return u

mapCons :: (Object -> SchemeM Object) -> Object -> SchemeM Object
mapCons _ ONil = return ONil
mapCons f (OCons a d) = OCons <$> f a <*> mapCons f d
mapCons _ o = throwError $
	"*** ERROR: proper list required for function application or macro use: "
	++ showObj o

foldlCons :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
foldlCons _ o0 ONil = return o0
foldlCons f o0 (OCons a d) = flip (foldlCons f) d =<< f o0 a
foldlCons _ _ _ = throwError "foldlCons: bad"

testEnv :: Env Object
testEnv = [
	("+", OSubr "+" $ foldlCons add (OInt 0)),
	("-", OSubr "-" subAll),
	("*", OSubr "*" $ foldlCons mul (OInt 1)),
	("exit", OSubr "exit" exit)
 ]

add :: Object -> Object -> SchemeM Object
add (OInt i1) (OInt i2) = return $ OInt $ i1 + i2
add x y = throwError $ "*** ERROR: operation + is not defined between " ++
	showObj x ++ " and " ++ showObj y

mul :: Object -> Object -> SchemeM Object
mul (OInt i1) (OInt i2) = return $ OInt $ i1 * i2
mul x y = throwError $ "*** ERROR: operation * is not defined between " ++
	showObj x ++ " and " ++ showObj y

subAll :: Object -> SchemeM Object
subAll (OCons (OInt i) ONil) = return $ OInt $ - i
subAll (OCons o ONil) =
	throwError $ "*** ERROR: operation - is not defined on object " ++ showObj o
subAll (OCons i0 is) = foldlCons sub i0 is
subAll ONil = throwError $
	"*** ERROR: procedure requires at least one argument: (-)"
subAll o = throwError $
	"*** ERROR: proper list required for function application or macro use: " ++
	showObj o

sub :: Object -> Object -> SchemeM Object
sub (OInt i1) (OInt i2) = return $ OInt $ i1 - i2
sub x y = throwError $ "*** ERROR: operation - is not defined between " ++
	showObj x ++ " and " ++ showObj y

-- div' :: Object -> Object -> Either String Object
-- div' (OInt i1) (OInt i2) = 


exit :: Object -> SchemeM Object
exit ONil = liftIO exitSuccess >> return OUndef
exit (OCons (OInt ec) ONil)
	| ec == 0 = exit ONil
	| otherwise = liftIO (exitWith $ ExitFailure $ fromIntegral ec) >>
		return OUndef
exit _ = throwError "*** ERROR: bad arguments"
