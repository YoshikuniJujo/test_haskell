{-# LANGUAGE QuasiQuotes, PackageImports, RankNTypes #-}

module Main where

import Text.Papillon
import Data.Char
import System.IO
-- import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Ratio

type SchemeM = StateT Env (ErrorT Err IO)
type Err = String

main :: IO ()
main = do
	_ <- runErrorT $ flip runStateT initEnv $ do
		doWhile_ $ do
			ln <- liftIO $ do
				putStr "yjscheme> "
				hFlush stdout
				getLine
			case ln of
				":q" -> return False
				_ -> do	case prs ln of
						Just p -> printObj $ eval p
						Nothing -> liftIO $ putStrLn 
							"parse error"
					return True
	return ()

type Env = [(String, Object)]

printObj :: SchemeM Object -> SchemeM ()
printObj o = catchError (o >>= liftIO . putStrLn . showObj) $ \e ->
	liftIO (putStrLn e)

initEnv :: Env
initEnv = [
	("+", OSubr "+" $ foldListl (iop (+)) $ OInt 0),
	("-", OSubr "-" sub),
	("*", OSubr "*" $ foldListl (iop (*)) $ OInt 1),
	("/", OSubr "/" div'),
	("quote", OSyntax "quote" car)
 ]

iop :: (forall a . Num a => a -> a -> a) -> Object -> Object -> SchemeM Object
iop op (OInt n) (OInt m) = return $ OInt $ n `op` m
iop op (ODouble d) (ODouble e) = return $ ODouble $ d `op` e
iop op n@(OInt _) e@(ODouble _) = flip (iop op) e =<< castToDouble n
iop op d@(ODouble _) m@(OInt _) = iop op d =<< castToDouble m
iop _ x y = throwError $ "iop: bad " ++ showObj x ++ " " ++ showObj y

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
castToRational _ = throwError $ "castToRational: can't cast"

castToDouble :: Object -> SchemeM Object
castToDouble (OInt n) = return $ ODouble $ fromIntegral n
castToDouble (ORational r) = return $ ODouble $ fromRational r
castToDouble d@(ODouble _) = return d
castToDouble x = throwError $ "castToDouble: can't cast: " ++ show x

car :: Object -> SchemeM Object
car (OCons a _) = return a
car _ = throwError "car: not cons "

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = do
	b <- act
	if b then doWhile_ act else return ()

eval :: Object -> SchemeM Object
eval i@(OInt _) = return i
eval d@(ODouble _) = return d
eval (OVar var) = do
	mval <- gets $ lookup var
	case mval of
		Just val -> return val
		Nothing -> throwError $ "*** ERROR: unbound variable: " ++ var
eval o@(OCons f_ args_) = do
	f <- eval f_
	case f of
		OSubr _ s -> s =<< mapList eval args_
		OSyntax _ s -> s args_
		_ -> throwError $ "*** ERROR: invalid application: " ++ showObj o
eval ONil = return ONil
eval _ = throwError "eval: not yet constructed"

mapList :: (Object -> SchemeM Object) -> Object -> SchemeM Object
mapList _ ONil = return ONil
mapList f (OCons a d) = OCons <$> f a <*> mapList f d
mapList _ _ = throwError "mapList: not list"

foldListl :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
foldListl _ o0 ONil = return o0
foldListl f o0 (OCons a d) = flip (foldListl f) d =<< f o0 a
foldListl _ _ _ = throwError "foldListl: not list"

prs :: String -> Maybe Object
prs src = case runError $ scm $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

data Object
	= OInt Integer
	| ORational Rational
	| ODouble Double
	| OVar String
	| OCons Object Object
	| ONil
	| OSubr String (Object -> SchemeM Object)
	| OSyntax String (Object -> SchemeM Object)

instance Show Object where
	show (ODouble d) = "(ODouble " ++ show d ++ ")"
	show _ = "show Object: yet"

showObj :: Object -> String
showObj (OInt i) = show i
showObj (ORational r) = show (numerator r) ++ "/" ++ show (denominator r)
showObj (ODouble d) = show d
showObj (OVar v) = v
showObj (OSubr n _) = "#<subr " ++ n ++ ">"
showObj (OSyntax n _) = "#<syntax " ++ n ++ ">"
showObj (OCons (OVar "quote") (OCons a ONil)) = "'" ++ showObj a
showObj c@(OCons _ _) = showCons False c
showObj ONil = "()"

showCons :: Bool -> Object -> String
showCons l (OCons a d) = (if l then id else ("(" ++) . (++ ")")) $
	case d of
		OCons _ _ -> showObj a ++ " " ++ showCons True d
		ONil -> showObj a
		_ -> showObj a ++ " . " ++ showObj d
showCons l ONil = if l then "" else "()"
showCons _ _ = error "not cons"

data Tkn
	= TIntL Integer
	| TDoubleL Double
	| TVar String
	| TOParen
	| TCParen
	| TDot
	| TQuote

isVar :: Char -> Bool
isVar = (||) <$> isAlpha <*> (`elem` "+-*/")

[papillon|

scm :: Object
	= o:obj _:spaces !_	{ o }

obj :: Object
	= (TIntL i):lx		{ OInt i }
	/ (TDoubleL d):lx	{ ODouble d }
	/ (TVar v):lx		{ OVar v }
	/ TOParen:lx os:obj* TCParen:lx
				{ foldr OCons ONil os }
	/ TOParen:lx as:obj* TDot:lx d:obj TCParen:lx
				{ foldr OCons d as }
	/ TQuote:lx o:obj	{ OCons (OVar "quote") $ OCons o ONil}

lx :: Tkn
	= _:spaces w:word	{ w }

word :: Tkn
	= n:<isDigit>+ '.' d:<isDigit>+
				{ TDoubleL $ read $ n ++ "." ++ d }
	/ ds:<isDigit>+		{ TIntL $ read ds }
	/ v:<isVar>+		{ TVar v }
	/ '('			{ TOParen }
	/ ')'			{ TCParen }
	/ '.'			{ TDot }
	/ '\''			{ TQuote }

spaces = _:<isSpace>*

|]
