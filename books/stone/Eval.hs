{-# Language PackageImports #-}

module Eval (
	eval,
	stoneParse,
	printObject
) where

import Parser
import Examples
import Data.Maybe
import Control.Applicative
import "monads-tf" Control.Monad.State

data Object
	= ONumber Int
	| OString String
	| OBool Bool
	| ONULL
	deriving Show

printObject :: Object -> IO ()
printObject = putStrLn . showObject

showObject :: Object -> String
showObject (ONumber n) = show n
showObject ONULL = "()"
showObject o = error $ "showObject: " ++ show o

type Env = [(String, Object)]

putValue :: String -> Object -> Env -> Env
putValue var val = ((var, val) :)

getValue :: String -> Env -> Object
getValue var = fromJust . lookup var

type Eval = State Env

eval :: Program -> Eval [Object]
eval = mapM evalStatement

evalStatement :: Statement -> Eval Object
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

evalPrimary :: Primary -> Eval Object
evalPrimary (PNumber n) = return $ ONumber n
evalPrimary (PIdentifier i) = gets $ getValue i
evalPrimary (PString s) = return $ OString s
evalPrimary (POp _) = error "evalPrimary: can't eval"
evalPrimary (PInfix (PIdentifier i) "=" p) = do
	r <- evalPrimary p
	modify $ putValue i r
	return r
evalPrimary (PInfix pl o pr) = do
	l <- evalPrimary pl
	r <- evalPrimary pr
	return $ getOp o l r

getOp :: String -> Object -> Object -> Object
getOp "+" (ONumber l) (ONumber r) = ONumber $ l + r
getOp "-" (ONumber l) (ONumber r) = ONumber $ l - r
getOp "*" (ONumber l) (ONumber r) = ONumber $ l * r
getOp "%" (ONumber l) (ONumber r) = ONumber $ l `mod` r
getOp ">" (ONumber l) (ONumber r) = OBool $ l > r
getOp "<" (ONumber l) (ONumber r) = OBool $ l < r
getOp "==" (ONumber l) (ONumber r) = OBool $ l == r
getOp op _ _ = error $ "getOp: " ++ op ++ " yet defined"
