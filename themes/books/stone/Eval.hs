{-# Language PackageImports #-}

module Eval (
	eval,
	stoneParse,
	printObject,
	initialEnv
) where

import Parser
import Examples
import Env

import Data.Maybe
import Data.Time
import Data.Array
import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Identity

type Eval = StateT (Env IO Block) IO
type Obj = Object IO Block

eval :: Program -> Eval [Obj]
eval = mapM evalStatement

evalStatement :: Statement -> Eval Obj
evalStatement (If e tb eb) = do
	b <- evalPrimary e
	case b of
		OBool False -> maybe (return ONULL)
			(fmap last . mapM evalStatement) eb
		_ -> last <$> mapM evalStatement tb
evalStatement (While e blk) = do
	b <- evalPrimary e
	case b of
		OBool False -> return ONULL
		_ -> do	mapM evalStatement blk
			evalStatement (While e blk)
evalStatement (Expr e) = evalPrimary e

evalPrimary :: Primary -> Eval Obj
evalPrimary (PNumber n) = return $ ONumber n
evalPrimary (PIdentifier i) = getValue i
evalPrimary (PString s) = return $ OString s
evalPrimary (POp _) = error "evalPrimary: can't eval"
evalPrimary (PInfix (PIdentifier i) "=" p) = do
	r <- evalPrimary p
	putValueG i r
	return r
evalPrimary (PInfix (PDot px i) "=" p) = do
	OObject eid <- evalPrimary px
	r <- evalPrimary p
	intoClosureEnv eid
	putValue i r
	exitClosureEnv eid
	return r
evalPrimary (PInfix (PIndex pa pi) "=" px) = do
	OArray aid <- evalPrimary pa
	ONumber i <- evalPrimary pi
	x <- evalPrimary px
	putToArray aid i x
	return x
	
evalPrimary (PInfix pl o pr) = do
	l <- evalPrimary pl
	r <- evalPrimary pr
	return $ getOp o l r
evalPrimary (PFunction f) = return $ OFunction f
evalPrimary (PApply fp args) = do
	f <- evalPrimary fp
	case f of
		OFunction (ps, blk) -> do
			vs <- mapM evalPrimary args
			newEnv
			zipWithM_ putValue ps vs
			rs <- eval blk
			popEnv
			return $ last rs
		OClosure eid (ps, blk) -> do
			vs <- mapM evalPrimary args
			intoClosureEnv eid
			zipWithM_ putValue ps vs
			rs <- eval blk
			exitClosureEnv eid
			return $ last rs
		ONative f -> lift . f =<< mapM evalPrimary args
evalPrimary (PClosure f) = do
	eid <- newCEnv
	return $ OClosure eid f
evalPrimary (PClass s b) = return $ OClass s b
evalPrimary (PDot pc "new") = do
	OClass s b <- evalPrimary pc
	eid <- newCEnv
	runClass s
	eval b
	return $ OObject eid
evalPrimary (PDot po m) = do
	OObject eid <- evalPrimary po
	intoClosureEnv eid
	o <- evalPrimary (PIdentifier m)
	exitClosureEnv eid
	return o
evalPrimary (PArray ps) = do
	os <- mapM evalPrimary ps
	aid <- newArray $ listArray (0, fromIntegral $ length os - 1) os
	return $ OArray aid
evalPrimary (PIndex pa pi) = do
	OArray a <- evalPrimary pa
	ONumber i <- evalPrimary pi
	arr <- getArray a
	return $ arr ! i

runClass :: Maybe Identifier -> Eval ()
runClass (Just sn) = do
	OClass s b' <- evalPrimary (PIdentifier sn)
	runClass s
	eval b'
	return ()
runClass _ = return ()

getOp :: String -> Obj -> Obj -> Obj
getOp "+" (ONumber l) (ONumber r) = ONumber $ l + r
getOp "+" (OString s1) (OString s2) = OString $ s1 ++ s2
getOp "+" (ONumber n) (OString s) = OString $ show n ++ s
getOp "+" (OString s) (ONumber n) = OString $ s ++ show n
getOp "-" (ONumber l) (ONumber r) = ONumber $ l - r
getOp "*" (ONumber l) (ONumber r) = ONumber $ l * r
getOp "%" (ONumber l) (ONumber r) = ONumber $ l `mod` r
getOp ">" (ONumber l) (ONumber r) = OBool $ l > r
getOp "<" (ONumber l) (ONumber r) = OBool $ l < r
getOp "==" (ONumber l) (ONumber r) = OBool $ l == r
getOp op _ _ = error $ "getOp: " ++ op ++ " yet defined"

initialEnv :: Env IO Block
initialEnv = mkInitialEnv [
	("print", ONative nfPrint),
	("currentTime", ONative currentTime)
 ]

nfPrint, currentTime :: [Obj] -> IO Obj
nfPrint [OString s] = putStrLn s >> return ONULL
nfPrint [ONumber n] = print n >> return ONULL
currentTime [] = do
	t <- getCurrentTime
	return $ ONumber $ diffTimeToMSec $ diffUTCTime t baseTime

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 1980 1 1) 0

diffTimeToMSec :: NominalDiffTime -> Integer
diffTimeToMSec = round . (* 1000)
