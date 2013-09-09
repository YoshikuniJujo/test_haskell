{-# LANGUAGE PackageImports, RankNTypes #-}

module Subrs (
	Environment, mkInitEnv,
	SchemeM, runEnvT, runSchemeM,
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
	rndm,
	runtime,
	quote,
	lets,
	errors,
	sins,
	coss,
	logs,
	exps,
	conss,
	cars,
	cdrs,
	list,
	nulls,
) where

import Eval

import "monads-tf" Control.Monad.Trans
import "monads-tf" Control.Monad.Reader
import Control.Applicative
import System.Exit
import System.Random
import Data.Ratio
import Data.Bits
import Data.Time

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
numOp _ op (ORational r) (ORational s) = return $ ORational $ r `op` s
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
def o = throwError . ("*** ERROR: syntax-error: " ++) . showObj =<<
	cons (OVar "define") o

lambda :: Maybe String -> Object -> SchemeM Object
lambda n (OCons as bd) = do
	eid <- getEID
	return $ OClosure n eid as bd
lambda _ o = throwError . ("*** ERROR: malformed lambda: " ++) . showObj =<<
	cons (OVar "lambda") o

cond :: Object -> SchemeM Object
cond (OCons (OCons (OVar "else") proc) ONil) = lastCons =<< mapCons eval proc
cond (OCons (OCons test proc) rest) = do
	t <- eval test
	case t of
		OBool False -> cond rest
		_ -> lastCons =<< mapCons eval proc
cond ONil = return OUndef
cond o = throwError . ("*** ERROR: syntax-error: " ++) . showObj =<<
	cons (OVar "cond") o

bopSeq :: String -> (forall a . Ord a => a -> a -> Bool) -> Object -> SchemeM Object
bopSeq _ op o@(OCons _ d) = andCons <$> zipWithCons (preCast $ bop op) o d
bopSeq n _ o = throwError . (("*** ERROR: wrong number of arguments for #<subr " ++
	n ++ ">: ") ++) . showObj =<< cons (OVar n) o

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
ifs o = throwError . ("*** ERROR: syntax-error: malformed if: " ++) .
	showObj =<< cons (OVar "if") o

ands, ors, nots :: Object -> SchemeM Object
ands (OCons x r) = do
	ret <- eval x
	case ret of
		OBool False -> return ret
		_ -> case r of
			ONil -> return ret
			_ -> ands r
ands ONil =  return $ OBool True
ands o = throwError .
	("*** ERROR: proper list required for function application of macro use: "
	++) . showObj =<< cons (OVar "and") o

ors (OCons x r) = do
	ret <- eval x
	case ret of
		OBool False -> ors r
		_ -> return ret
ors ONil = return $ OBool False
ors o = throwError .
	("*** ERROR: proper list required for function application of macro use: "
	++) . showObj =<< cons (OVar "or") o

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
quotient o = throwError . ("*** ERROR: integer required: " ++) . showObj =<<
	cons (OVar "quotient") o
remainder (OCons (OInt i) (OCons (OInt j) ONil)) = return $ OInt $ i `rem` j
remainder o = throwError . ("*** ERROR: integer required: " ++) . showObj =<<
	cons (OVar "remainder") o

logbit :: Object -> SchemeM Object
logbit (OCons (OInt i) (OCons (OInt j) ONil)) =
	return $ OBool $ j `testBit` fromIntegral i
logbit o = throwError . ("*** ERROR: integer required: " ++) . showObj =<<
	cons (OVar "logbit") o

rndm :: Object -> SchemeM Object
rndm (OCons (OInt i) ONil) = liftIO $ OInt <$> randomRIO (0, i - 1)
rndm o = throwError . ("*** ERROR: wrong number or wrong type of arguments: " ++) .
	showObj =<< cons (OVar "random") o

runtime :: Object -> SchemeM Object
runtime ONil = do
	b <- ask
	n <- liftIO getCurrentTime
	let d = diffUTCTime n b
	return $ OInt $ floor $ d * 1000000
runtime o = throwError . ("*** ERROR: " ++) . showObj =<< cons (OVar "runtime") o

quote :: Object -> SchemeM Object
quote (OCons o ONil) = return o
quote o = throwError . ("*** ERROR: malformed quote: " ++) . showObj =<<
	cons (OVar "quote") o

lets :: Object -> SchemeM Object
lets o = do
	err <- ("*** ERROR: syntax-error: malformed let: " ++) . showObj <$>
		cons (OVar "let") o
	vvs <- car err o
	body <- cdr err o
	vars <- mapCons (car err) vvs
	vals <- mapCons ((car err =<<) . cdr err) vvs
	eval =<< flip cons vals =<< cons (OVar "lambda") =<< cons vars body

errors :: Object -> SchemeM Object
errors o = throwError . ("*** ERROR: " ++) . unwords . map showObj =<< cons2list o

dfun :: String -> (Double -> Double) -> Object -> SchemeM Object
dfun n f o = do
	l <- cons2list o
	case l of
		[ODouble d] -> return $ ODouble $ f d
		[OInt i] -> return $ ODouble $ f $ fromIntegral i
		[ORational r] -> return $ ODouble $ f $ fromRational r
		_ -> throwError .
			(("wrong number or types of arguments for #<subr " ++ n ++
			"> :") ++) . showObj =<< cons (OVar n) o

sins, coss, logs, exps :: Object -> SchemeM Object
sins = dfun "sin" sin
coss = dfun "cos" cos
logs = dfun "log" log
exps = dfun "exp" exp

conss :: Object -> SchemeM Object
conss o = do
	l <- cons2list o
	case l of
		[a, b] -> cons a b
		_ -> throwError .
			("*** ERROR: wrong number of arguments: cons requires 2: " ++) .
			showObj =<< cons (OVar "cons") o

cars, cdrs :: Object -> SchemeM Object
cars o = do
	l <- cons2list o
	emsg <- ("*** ERROR: wrong number or types of arguments: car: " ++) .
		showObj <$> cons (OVar "car") o
	case l of
		[o'] -> car emsg o'
		_ -> throwError emsg
cdrs o = do
	l <- cons2list o
	oo <- cons (OVar "cdr") o
	let emsg = "*** ERROR: wrong number or types of arguments: cdr: " ++
		showObj oo
	case l of
		[o'] -> cdr emsg o'
		_ -> throwError emsg

list :: Object -> SchemeM Object
list = return

nulls :: Object -> SchemeM Object
nulls o = do
	l <- cons2list o
	case l of
		[ONil] -> return $ OBool True
		[_] -> return $ OBool False
		_ -> throwError .
			("*** ERROR: wrong number of arguments: null?: " ++) .
			showObj =<< cons (OVar "null?") o
