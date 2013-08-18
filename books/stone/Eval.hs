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
import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Identity

type Eval = StateT (Env IO Function) IO
type Obj = Object IO Function

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

initialEnv :: Env IO Function
initialEnv = mkInitialEnv [
	("print", ONative nfPrint),
	("currentTime", ONative currentTime)
 ]

nfPrint, currentTime :: [Obj] -> IO Obj
nfPrint [OString s] = putStrLn s >> return ONULL
currentTime [] = do
	t <- getCurrentTime
	return $ ONumber $ diffTimeToMSec $ diffUTCTime t baseTime

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 1980 1 1) 0

diffTimeToMSec :: NominalDiffTime -> Integer
diffTimeToMSec = round . (* 1000)
