{-# LANGUAGE PackageImports, RankNTypes #-}

module Subrs (
	Environment, mkInitEnv,
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
	def,
	lambda,
	cond,
	bopSeq,
	ifs,
	ands,
	ors,
	nots,
	display,
	expt,
	quotient,
	remainder,
	logbit,
) where

import Eval

import "monads-tf" Control.Monad.Trans
import Control.Applicative
import System.Exit
import Data.Ratio
import Data.Bits

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

expt :: Object -> SchemeM Object
expt (OCons (OInt i) (OCons (OInt j) ONil)) = return $ OInt $ i ^ j
expt (OCons (ORational r) (OCons (OInt j) ONil)) = return $ ORational $ r ^ j
expt (OCons (ODouble d) (OCons (OInt j) ONil)) = return $ ODouble $ d ^ j
expt (OCons (OInt i) (OCons (ODouble e) ONil)) =
	return $ ODouble $ fromIntegral i ** e
expt (OCons (ORational r) (OCons (ODouble e) ONil)) =
	return $ ODouble $ fromRational r ** e
expt (OCons (ODouble d) (OCons (ODouble e) ONil)) = return $ ODouble $ d ** e
expt (OCons n (OCons (ORational s) ONil)) =
	expt (OCons n (OCons (ODouble $ fromRational s) ONil))
expt _ = throwError "*** ERROR: wrong number of arguments for #<closure expt>"

def :: Object -> SchemeM Object
def (OCons v@(OVar var) (OCons val ONil)) = do
	r <- eval val
	case r of
		OClosure Nothing eid as bd ->
			define var $ OClosure (Just var) eid as bd
		_ -> define var r
	return v
def (OCons (OCons fn@(OVar n) as) bd) =
	def $ OCons fn $ OCons
		(OCons (OSyntax "lambda" $ lambda $ Just n) $ OCons as bd) ONil
def o = throwError $ "*** ERROR: syntax-error: " ++
	showObj (OCons (OVar "define") o)

lambda :: Maybe String -> Object -> SchemeM Object
lambda n (OCons as bd) = do
	eid <- getEID
	return $ OClosure n eid as bd
lambda _ o = throwError $ "*** ERROR: malformed lambda: " ++
	showObj (OCons (OVar "lambda") o)

cond :: Object -> SchemeM Object
cond (OCons (OCons (OVar "else") proc) ONil) = lastCons =<< mapCons eval proc
cond (OCons (OCons test proc) rest) = do
	t <- eval test
	case t of
		OBool False -> cond rest
		_ -> lastCons =<< mapCons eval proc
cond ONil = return OUndef
cond o = throwError $ "*** ERROR: syntax-error: " ++
	showObj (OCons (OVar "cond") o)

bopSeq :: String -> (forall a . Ord a => a -> a -> Bool) -> Object -> SchemeM Object
bopSeq _ op o@(OCons _ d) = andCons <$> zipWithCons (preCast $ bop op) o d
bopSeq n _ o = throwError $ "*** ERROR: wrong number of arguments for #<subr " ++
	n ++ ">: " ++ showObj (OCons (OVar n) o)

bop :: (forall a . Ord a => a -> a -> Bool) -> Object -> Object -> SchemeM Object
bop op (OInt i) (OInt j) = return $ OBool $ i `op` j
bop op (ODouble d) (ODouble e) = return $ OBool $ d `op` e
bop op (ORational r) (ORational s) = return $ OBool $ r `op` s
bop _ x y = throwError $ "bop: " ++ showObj x ++ " " ++ showObj y

andCons :: Object -> Object
andCons (OCons (OBool True) d) = andCons d
andCons (OCons (OBool False) _) = OBool False
andCons ONil = OBool True
andCons _ = error $ "andCons: bad"

ifs :: Object -> SchemeM Object
ifs (OCons test (OCons thn (OCons els ONil))) = do
	t <- eval test
	case t of
		OBool False -> eval els
		_ -> eval thn
ifs (OCons test (OCons thn ONil)) = do
	t <- eval test
	case t of
		OBool False -> return OUndef
		_ -> eval thn
ifs o = throwError $ "*** ERROR: syntax-error: malformed if: " ++
	showObj (OCons (OVar "if") o)

ands, ors, nots :: Object -> SchemeM Object
ands (OCons x r) = do
	ret <- eval x
	case ret of
		OBool False -> return ret
		_ -> case r of
			ONil -> return ret
			_ -> ands r
ands ONil =  return $ OBool True
ands o = throwError $
	"*** ERROR: proper list required for function application of macro use: "
	++ showObj (OCons (OVar "and") o)

ors (OCons x r) = do
	ret <- eval x
	case ret of
		OBool False -> ors r
		_ -> return ret
ors ONil = return $ OBool False
ors o = throwError $
	"*** ERROR: proper list required for function application of macro use: "
	++ showObj (OCons (OVar "or") o)

nots (OCons (OBool False) ONil) = return $ OBool True
nots (OCons _ ONil) = return $ OBool False
nots _ = throwError $
	"*** ERROR: wrong number of arguments: not requires 1, but got x"

display :: Object -> SchemeM Object
display (OCons (OString s) ONil) = liftIO (putStr s) >> return OUndef
display (OCons v ONil) = liftIO (putStr $ showObj v) >> return OUndef
display _ = throwError $ "*** ERROR: not implemented yet"

quotient, remainder :: Object -> SchemeM Object
quotient (OCons (OInt i) (OCons (OInt j) ONil)) = return $ OInt $ i `div` j
quotient o = throwError $ "*** ERROR: integer required: " ++ showObj
	(OCons (OVar "quotient") o)
remainder (OCons (OInt i) (OCons (OInt j) ONil)) = return $ OInt $ i `rem` j
remainder o = throwError $ "*** ERROR: integer required: " ++ showObj
	(OCons (OVar "remainder") o)

logbit :: Object -> SchemeM Object
logbit (OCons (OInt i) (OCons (OInt j) ONil)) =
	return $ OBool $ j `testBit` fromIntegral i
logbit o = throwError $ "*** ERROR: integer required: " ++ showObj
	(OCons (OVar "logbit") o)
