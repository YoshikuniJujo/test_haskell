{-# LANGUAGE PackageImports, RankNTypes #-}

module Subrs (
	Environment, mkInitEnv,
	SchemeM, runEnvT, runSchemeM,
	Object(..),
	eval,
	throwError, catchError,

	foldlCons,
	cons2list,
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
	undef,
	isPair,
	apply,
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
exit o = do
	l <- cons2list o
	case l of
		[OInt 0] -> exit ONil
		[OInt ec] -> liftIO (exitWith $ ExitFailure $ fromIntegral ec) >>
			return OUndef
		_ -> throwError "*** ERROR: bad arguments"

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
oneOrMore n f op o = do
	l <- cons2list o
	case l of
		[o'] -> f o'
		o0 : os -> foldM op o0 os
		[] -> throwError $
			"*** ERROR: procedure requires at least one argument: (" ++ n ++ ")"

divide :: Object -> Object -> SchemeM Object
divide (OInt i) (OInt j) = let r = fromIntegral i / fromIntegral j in
	return $ if denominator r == 1 then OInt $ numerator r else ORational r
divide (ORational r) (ORational s) = let t = r / s in
	return $ if denominator t == 1 then OInt $ numerator t else ORational t
divide (ODouble d) (ODouble e) = return $ ODouble $ d / e
divide x y = throwError $ "*** ERROR: operation / is not defined between " ++
	showObj x ++ " and " ++ showObj y

expt :: Object -> SchemeM Object
expt o = do
	l <- cons2list o
	case l of
		[OInt i, OInt j] -> return $ OInt $ i ^ j
		[ORational r, OInt j] -> return $ ORational $ r ^ j
		[ODouble d, OInt j] -> return $ ODouble $ d ^ j
		[OInt i, ODouble e] -> return $ ODouble $ fromIntegral i ** e
		[ORational r, ODouble e] -> return $ ODouble $ fromRational r ** e
		[ODouble d, ODouble e] -> return $ ODouble $ d ** e
		[n, ORational s] -> do
			c <- cons n =<< cons (ODouble $ fromRational s) ONil
			expt c
		_ -> throwError
			"*** ERROR: wrong number of arguments for #<closure expt>"

def :: Object -> SchemeM Object
def o = do
	err <- ("*** ERROR: syntax-error: " ++) . showObj <$>
		cons (OVar "define") o
	a <- car err o
	d <- cdr err o
	case a of
		v@(OVar var) -> do
			r <- eval =<< car err d
			case r of
				OClosure Nothing eid as bd ->
					define var $ OClosure (Just var) eid as bd
				_ -> define var r
			return v
		_ -> do	fn <- car err a
			as <- cdr err a
			case fn of
				OVar n -> do
					l <- cons (OSyntax "lambda" $
						lambda $ Just n) =<< cons as d
					r <- cons fn =<< cons l ONil
					def r
				_ -> throwError err

lambda :: Maybe String -> Object -> SchemeM Object
lambda n o = do
	err <- ("*** ERROR: malformed lambda: " ++) . showObj <$>
		cons (OVar "lambda") o
	a <- car err o
	d <- cdr err o
	eid <- getEID
	return $ OClosure n eid a d

cond :: Object -> SchemeM Object
cond ONil = return OUndef
cond o = do
	err <- ("*** ERROR: syntax-error: " ++) . showObj <$> cons (OVar "cond") o
	a <- car err o
	d <- cdr err o
	case d of
		ONil -> do
			a' <- car err a
			d' <- cdr err a
			case a' of
				OVar "else" -> lastCons =<< mapCons eval d'
				test -> do
					t <- eval test
					case t of
						OBool False -> return OUndef
						_ -> lastCons =<< mapCons eval d'
		_ -> do	t <- eval =<< car err a
			d' <- cdr err a
			case t of
				OBool False -> cond d
				_ -> lastCons =<< mapCons eval d'

bopSeq :: String -> (forall a . Ord a => a -> a -> Bool) -> Object -> SchemeM Object
bopSeq n op o = do
	l <- cons2list o
	case l of
		oa@(_ : os) -> OBool . all isTrue <$> zipWithM (preCast $ bop op) oa os
		_ -> throwError . (("*** ERROR: wrong number of arguments for #<subr " ++
			n ++ ">: ") ++) . showObj =<< cons (OVar n) o

bop :: (forall a . Ord a => a -> a -> Bool) -> Object -> Object -> SchemeM Object
bop op (OInt i) (OInt j) = return $ OBool $ i `op` j
bop op (ODouble d) (ODouble e) = return $ OBool $ d `op` e
bop op (ORational r) (ORational s) = return $ OBool $ r `op` s
bop op (OBool b) (OBool c) = return $ OBool $ b `op` c
bop _ x y = throwError $ "bop: " ++ showObj x ++ " " ++ showObj y

isTrue :: Object -> Bool
isTrue (OBool False) = False
isTrue _ = True

ifs :: Object -> SchemeM Object
ifs o = do
	err <- ("*** ERROR: syntax-error: malformed if: " ++) .
		showObj <$> cons (OVar "if") o
	l <- cons2list o
	case l of
		[test, thn, els] -> do
			t <- eval test
			case t of
				OBool False -> eval els
				_ -> eval thn
		[test, thn] -> do
			t <- eval test
			case t of
				OBool False -> return OUndef
				_ -> eval thn
		_ -> throwError err

ands, ors, nots :: Object -> SchemeM Object
ands ONil =  return $ OBool True
ands o = do
	err <- ("*** ERROR: proper list required for function application of macro use: " ++) . showObj <$> cons (OVar "and") o
	ret <- eval =<< car err o
	d <- cdr err o
	case ret of
		OBool False -> return ret
		_ -> case d of
			ONil -> return ret
			_ -> ands d

ors ONil = return $ OBool False
ors o = do
	err <- ("*** ERROR: proper list required for function application of macro use: " ++) . showObj <$> cons (OVar "or") o
	ret <- eval =<< car err o
	d <- cdr err o
	case ret of
		OBool False -> ors d
		_ -> return ret

nots o = do
	l <- cons2list o
	case l of
		[OBool False] -> return $ OBool True
		[_] -> return $ OBool False
		_ -> throwError $
			"*** ERROR: wrong number of arguments: not requires 1, but got x"

display :: Object -> SchemeM Object
display o = do
	l <- cons2list o
	case l of
		[OString s] -> liftIO (putStr s) >> return OUndef
		[v] -> liftIO (putStr $ showObj v) >> return OUndef
		_ -> throwError $ "*** ERROR: not implemented yet"

quotient, remainder :: Object -> SchemeM Object
quotient o = do
	l <- cons2list o
	err <- ("*** ERROR: integer required: " ++) . showObj <$>
		cons (OVar "quotient") o
	case l of
		[OInt i, OInt j] -> return $ OInt $ i `div` j
		_ -> throwError err
remainder o = do
	l <- cons2list o
	err <- ("*** ERROR: integer required: " ++) . showObj <$>
		cons (OVar "remainder") o
	case l of
		[OInt i, OInt j] -> return $ OInt $ i `rem` j
		_ -> throwError err

logbit :: Object -> SchemeM Object
logbit o = do
	l <- cons2list o
	err <- ("*** ERROR: integer required: " ++) . showObj <$>
		cons (OVar "logbit") o
	case l of
		[OInt i, OInt j] -> return $ OBool $ j `testBit` fromIntegral i
		_ -> throwError err

rndm :: Object -> SchemeM Object
rndm o = do
	l <- cons2list o
	err <- ("*** ERROR: wrong number or wrong type of arguments: " ++) .
		showObj <$> cons (OVar "random") o
	case l of
		[OInt i] -> liftIO $ OInt <$> randomRIO (0, i - 1)
		_ -> throwError err

runtime :: Object -> SchemeM Object
runtime ONil = do
	b <- ask
	n <- liftIO getCurrentTime
	let d = diffUTCTime n b
	return $ OInt $ floor $ d * 1000000
runtime o = throwError . ("*** ERROR: " ++) . showObj =<< cons (OVar "runtime") o

quote :: Object -> SchemeM Object
quote o = do
	l <- cons2list o
	err <- ("*** ERROR: malformed quote: " ++) . showObj <$>
		cons (OVar "quote") o
	case l of
		[o'] -> return o'
		_ -> throwError err

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
	emsg <- ("*** ERROR: wrong number or types of arguments: car: " ++) <$>
		showObjM o
--		showObj <$> cons (OVar "car") o
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

undef :: Object -> SchemeM Object
undef ONil = return OUndef
undef _ = throwError $
	"*** ERROR: wrong number of arguments: undefined requires 0, but got some"

isPair :: Object -> SchemeM Object
isPair o = do
	l <- cons2list o
	case l of
		[o'] -> OBool <$> isCons o'
		_ -> throwError "*** ERROR: isPair bad"

apply :: Object -> SchemeM Object
apply o = do
	l <- cons2list o
	case l of
		[f, as] -> eval =<< cons f as
		_ -> throwError "*** ERROR: apply"
