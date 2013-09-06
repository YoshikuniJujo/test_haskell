{-# LANGUAGE RankNTypes, PackageImports #-}

module InitEnv (initEnv) where

import Eval

import System.Exit
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Applicative
import Control.Arrow
import Data.Ratio
import Data.Maybe

initEnv :: Env
initEnv = [
	(EVar "+", OSubr "+" $ foldListl (iop (+)) $ OInt 0),
	(EVar "-", OSubr "-" sub),
	(EVar "*", OSubr "*" $ foldListl (iop (*)) $ OInt 1),
	(EVar "/", OSubr "/" div'),
	(EVar "<", OSubr "<" $ bopSeq (<)),
	(EVar ">", OSubr ">" $ bopSeq (>)),
	(EVar "=", OSubr "=" $ bopSeq (==)),
--	(EVar "and", OSubr "and" $ foldListl (bbop (&&)) $ OBool True),
--	(EVar "or", OSubr "or" $ foldListl (bbop (||)) $ OBool False),
	(EVar "and", OSyntax "and" ands),
	(EVar "or", OSyntax "or" ors),
	(EVar "not", OSubr "not" nots),
	(EVar "quote", OSyntax "quote" car),
	(EVar "define", OSyntax "define" define),
	(EVar "display", OSubr "display" display),
	(EVar "exit", OSubr "exit" exit),
	(EVar "lambda", OSyntax "lambda" $ lambda Nothing),
	(EVar "cond", OSyntax "cond" cond),
	(EVar "if", OSyntax "if" ifs)
 ]

iop :: (forall a . Num a => a -> a -> a) -> Object -> Object -> SchemeM Object
iop op (OInt n) (OInt m) = return $ OInt $ n `op` m
iop op (ODouble d) (ODouble e) = return $ ODouble $ d `op` e
iop op (ORational r) (ORational s) = return $ ORational $ r `op` s
iop op n@(OInt _) e@(ODouble _) = flip (iop op) e =<< castToDouble n
iop op d@(ODouble _) m@(OInt _) = iop op d =<< castToDouble m
iop op n@(OInt _) s@(ORational _) = flip (iop op) s =<< castToRational n
iop op r@(ORational _) m@(OInt _) = iop op r =<< castToRational m
iop op d@(ODouble _) s@(ORational _) = iop op d =<< castToDouble s
iop op r@(ORational _) e@(ODouble _) = flip (iop op) e =<< castToDouble r
iop _ x y = throwError $ "iop: bad " ++ showObj x ++ " " ++ showObj y

bopSeq :: (forall a . Ord a => a -> a -> Bool) -> Object -> SchemeM Object
bopSeq op o@(OCons _ d) = andList <$> zipWithList (bop op) o d
bopSeq _ _ = throwError $ "bopSeq: bad"

bop :: (forall a . Ord a => a -> a -> Bool) -> Object -> Object -> SchemeM Object
bop op (OInt n) (OInt m) = return $ OBool $ n `op` m
bop op (ODouble d) (ODouble e) = return $ OBool $ d `op` e
bop op (ORational r) (ORational s) = return $ OBool $ r `op` s
bop op d@(ODouble _) m@(OInt _) = bop op d =<< castToDouble m
bop _ x y = throwError $ "bop: bad " ++ showObj x ++ " " ++ showObj y

{-
bbop :: (Bool -> Bool -> Bool) -> Object -> Object -> SchemeM Object
bbop op (OBool b) (OBool c) = return $ OBool $ b `op` c
bbop _ _ _ = throwError $ "bbop: bad"
-}

ands :: Object -> SchemeM Object
ands (OCons x r) = do
	ret <- eval x
	case ret of
		OBool False -> return $ OBool False
		_ -> case r of
			ONil -> return ret
			_ -> ands r
ands ONil = return $ OBool True
ands o = throwError $ "*** ERROR: proper list required for function application or macro use: " ++
	showObj (OCons (OVar "and") o)

ors :: Object -> SchemeM Object
ors (OCons x r) = do
	ret <- eval x
	case ret of
		OBool False -> ors r
		_ -> return ret
ors ONil = return $ OBool False
ors o = throwError $ "*** ERROR: proper list required for function application or macro use: " ++
	showObj (OCons (OVar "or") o)

nots :: Object -> SchemeM Object
nots (OCons (OBool False) ONil) = return $ OBool True
nots (OCons _ ONil) = return $ OBool False
nots _ = throwError $ "*** ERROR: wrong number of arguments: not requires 1, but got x"

andList :: Object -> Object
andList (OCons (OBool True) d) = andList d
andList (OCons (OBool False) _) = OBool False
andList ONil = OBool True
andList _ = error $ "andList: bad"

rop :: (forall a . Fractional a => a -> a -> a) -> Object -> Object -> SchemeM Object
rop op (ODouble d) (ODouble e) = return $ ODouble $ d `op` e
rop op (ORational n) (ORational m) = do
	let r = n `op` m
	if denominator r == 1
		then return $ OInt $ numerator r
		else return $ ORational r
rop _ _ _ = throwError "rop: bad"

sub :: Object -> SchemeM Object
sub (OCons (OInt n) ONil) = return $ OInt $ - n
sub (OCons (ODouble d) ONil) = return $ ODouble $ - d
sub (OCons (ORational r) ONil) = return $ ORational $ - r
sub (OCons n ms) = iop (-) n =<< foldListl (iop (+)) (OInt 0) ms
sub _ = throwError "sub: bad"

div' :: Object -> SchemeM Object
div' (OCons (OInt n) ONil) = return $ ORational $ recip $ fromIntegral n
div' (OCons (ODouble d) ONil) = return $ ODouble $ recip d
div' (OCons n@(ODouble _) ms) = do
	rop (/) n =<< castToDouble =<<
		foldListl (iop (*)) (OInt 1) ms
div' (OCons n ms) = do
	n' <- castToRational n
	rop (/) n' =<< castToRational =<<
		foldListl (iop (*)) (OInt 1) ms
div' _ = throwError "div': bad"

castToRational :: Object -> SchemeM Object
castToRational (OInt n) = return $ ORational $ fromIntegral n
castToRational (ODouble d) = return $ ORational $ toRational d
castToRational r@(ORational _) = return r
castToRational _ = throwError $ "castToRational: can't cast"

castToDouble :: Object -> SchemeM Object
castToDouble (OInt n) = return $ ODouble $ fromIntegral n
castToDouble (ORational r) = return $ ODouble $ fromRational r
castToDouble d@(ODouble _) = return d
castToDouble x = throwError $ "castToDouble: can't cast: " ++ show x

car :: Object -> SchemeM Object
car (OCons a _) = return a
car _ = throwError "car: not cons "

define :: Object -> SchemeM Object
define (OCons v@(OVar var) (OCons val ONil)) = do
	r <- eval val
	case r of
		OClosure fn_ eid as bd -> do
			let fn = Just $ fromMaybe var fn_
			defineVar var $ OClosure fn eid as bd
			neid <- nowEnv
			intoEnv eid
			defineVar var $ OClosure fn eid as bd
			intoEnv neid
			modify $ second ((EVar "hoge", OString "hage") :)
		{- modify $
			second ((EVar var, OClosure (Just var) eid as bd) :) -}
		_ -> defineVar var r -- modify $ second ((EVar var, r) :)
	return v
define (OCons (OCons fn@(OVar n) as) bd) =
	define $ OCons fn $ OCons
		(OCons (OSyntax "lambda" $ lambda $ Just n) $ OCons as bd) ONil
define o = throwError $ "*** ERROR: syntax-error: " ++ showObj o

display :: Object -> SchemeM Object
display (OCons (OString s) ONil) = liftIO (putStr s) >> return OUndef
display (OCons v ONil) = liftIO (putStr $ showObj v) >> return OUndef
display _ = throwError $ "*** ERROR: not implemented yet"

exit :: Object -> SchemeM Object
exit ONil = liftIO exitSuccess >> return OUndef
exit (OCons (OInt ec) ONil)
	| ec == 0 = exit ONil
	| otherwise = liftIO (exitWith $ ExitFailure $ fromIntegral ec) >>
		return OUndef
exit _ = throwError "*** ERROR: bad arguments"

lambda :: Maybe String -> Object -> SchemeM Object
lambda n (OCons as bd) = do
	eid <- newEnv
	return $ OClosure n eid as bd
lambda _ o = throwError $ "*** ERROR: malformed lambda: " ++ showObj o

cond :: Object -> SchemeM Object
cond (OCons (OCons (OVar "else") proc) ONil) =
	lastList <$> mapList eval proc
cond (OCons (OCons test proc) rest) = do
	t <- eval test
	case t of
		OBool False -> cond rest
		_ -> lastList <$> mapList eval proc
cond ONil = return OUndef
cond o = throwError $ "*** ERROR: syntax-error: " ++
	showObj (OCons (OVar "cond") o)

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
