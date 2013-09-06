{-# LANGUAGE PackageImports, RankNTypes #-}

module Subrs (
	Environment, fromList,
	SchemeM, runEnvT,
	Object(..),
	eval,
	throwError, catchError,

	foldlCons,
	add,
	subAll,
	mul,
	divAll,
	exit,
	def
) where

import Eval

import "monads-tf" Control.Monad.Trans
import System.Exit
import Data.Ratio

add, mul, sub :: Object -> Object -> SchemeM Object
add = preCast $ numOp "+" (+)
mul = preCast $ numOp "*" (*)
sub = preCast $ numOp "-" (-)

subAll, divAll :: Object -> SchemeM Object
subAll = oneOrMore "-" (numFun "-" negate) sub
divAll = oneOrMore "/" (fracFun "/" recip) $ preCast divide

exit :: Object -> SchemeM Object
exit ONil = liftIO exitSuccess >> return OUndef
exit (OCons (OInt ec) ONil)
	| ec == 0 = exit ONil
	| otherwise = liftIO (exitWith $ ExitFailure $ fromIntegral ec) >>
		return OUndef
exit _ = throwError "*** ERROR: bad arguments"

preCast :: (Object -> Object -> SchemeM Object) ->
	(Object -> Object -> SchemeM Object)
preCast op x y = uncurry op $ castNum2 x y

castNum2 :: Object -> Object -> (Object, Object)
castNum2 (OInt i) s@(ORational _) = (ORational $ fromIntegral i, s)
castNum2 r@(ORational _) (OInt j) = (r, ORational $ fromIntegral j)
castNum2 (OInt i) e@(ODouble _) = (ODouble $ fromIntegral i, e)
castNum2 d@(ODouble _) (OInt j) = (d, ODouble $ fromIntegral j)
castNum2 (ORational r) e@(ODouble _) = (ODouble $ fromRational r, e)
castNum2 d@(ODouble _) (ORational s) = (d, ODouble $ fromRational s)
castNum2 x y = (x, y)

numFun :: String -> (forall a . Num a => a -> a) -> Object -> SchemeM Object
numFun _ f (OInt i) = return $ OInt $ f i
numFun _ f (ORational r) = return $ ORational $ f r
numFun _ f (ODouble d) = return $ ODouble $ f d
numFun n _ x = throwError $
	"*** ERROR: operation " ++ n ++ " is not defined on object " ++ showObj x

fracFun :: String -> (forall a . Fractional a => a -> a) -> Object -> SchemeM Object
fracFun _ f (OInt i) = return $ ORational $ f $ fromIntegral i
fracFun _ f (ORational r) = return $ ORational $ f r
fracFun _ f (ODouble d) = return $ ODouble $ f d
fracFun n _ x = throwError $
	"*** ERROR: operation " ++ n ++ " is not defined on object " ++ showObj x

numOp :: String -> (forall a . Num a => a -> a -> a) -> Object -> Object ->
	SchemeM Object
numOp _ op (OInt i) (OInt j) = return $ OInt $ i `op` j
numOp _ op (ODouble d) (ODouble e) = return $ ODouble $ d `op` e
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

divide :: Object -> Object -> SchemeM Object
divide (OInt i) (OInt j) = let r = fromIntegral i / fromIntegral j in
	return $ if denominator r == 1 then OInt $ numerator r else ORational r
divide (ORational r) (ORational s) = let t = r / s in
	return $ if denominator t == 1 then OInt $ numerator t else ORational t
divide (ODouble d) (ODouble e) = return $ ODouble $ d / e
divide x y = throwError $ "*** ERROR: operation / is not defined between " ++
	showObj x ++ " and " ++ showObj y

def :: Object -> SchemeM Object
def (OCons v@(OVar var) (OCons val ONil)) = do
	r <- eval val
	define var r
	return v
def o = throwError $ "*** ERROR: syntax-error: " ++
	showObj (OCons (OVar "define") o)
