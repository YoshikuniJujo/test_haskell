{-# LANGUAGE QuasiQuotes, PackageImports, RankNTypes, TupleSections #-}

module Main where

import Text.Papillon
import Data.Char
import Data.Maybe
import Data.Ratio
import System.IO
import System.Exit
-- import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error

type SchemeM = StateT ((EID, Maybe (EID, Env)), Env) (ErrorT Err IO)
type Err = String
type Env = [(EKey, Object)]
data EKey = EVar String | EID EID deriving (Eq, Show)
type EID = Int
topEID :: EID
topEID = 0

nowEnv :: SchemeM EID
nowEnv = do
	((_, here), _) <- get
	return $ case here of
		Just (eid, _) -> eid
		_ -> 0

newEnv :: SchemeM EID
newEnv = do
	((maxID, here), tenv) <- get
	let	newID = succ maxID
	put ((newID, here), (EID newID, OEnv newID $ maybe [] snd here) : tenv)
	return $ newID

intoEnv :: EID -> SchemeM ()
intoEnv eid = do
	((maxID, here), tenv) <- get
	case here of
		Just (oeid, oenv) ->
			put ((maxID, (eid ,) . (\(OEnv _ e) -> e) <$>
					lookup (EID eid) tenv),
				(EID oeid, OEnv oeid oenv) : tenv)
		_ -> put ((maxID, (eid ,) . (\(OEnv _ e) -> e) <$>
					lookup (EID eid) tenv), tenv)

main :: IO ()
main = do
	_ <- runErrorT $ flip runStateT ((topEID, Nothing), initEnv) $ do
		doWhile_ $ do
			ln <- prompt 0 ""
			case prs ln of
				Just p -> printObj $ eval p
				Nothing -> liftIO $ putStrLn $ "parse error: " ++ ln
			return True
	return ()

prompt :: Int -> String -> SchemeM String
prompt d s = do
	n <- liftIO $ do
		putStr $ "yjscheme:" ++ show d ++ "> "
		hFlush stdout
		getLine
	let	s' = s ++ " " ++ n
		d' = dpt s'
	if maybe False (> 0) d'
		then prompt (fromJust d') s'
		else return s'

printObj :: SchemeM Object -> SchemeM ()
printObj o = catchError (o >>= liftIO . putStrLn . showObj) $ \e ->
	liftIO (putStrLn e)

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
	(EVar "load", OSubr "load" load),
	(EVar "top-env", OSubr "top-env" topEnv),
	(EVar "lambda", OSyntax "lambda" $ lambda Nothing),
	(EVar "cond", OSyntax "cond" cond),
	(EVar "if", OSyntax "if" ifs)
 ]

iop :: (forall a . Num a => a -> a -> a) -> Object -> Object -> SchemeM Object
iop op (OInt n) (OInt m) = return $ OInt $ n `op` m
iop op (ODouble d) (ODouble e) = return $ ODouble $ d `op` e
iop op n@(OInt _) e@(ODouble _) = flip (iop op) e =<< castToDouble n
iop op d@(ODouble _) m@(OInt _) = iop op d =<< castToDouble m
iop _ x y = throwError $ "iop: bad " ++ showObj x ++ " " ++ showObj y

bopSeq :: (forall a . Ord a => a -> a -> Bool) -> Object -> SchemeM Object
bopSeq op o@(OCons _ d) = andList <$> zipWithList (bop op) o d
bopSeq _ _ = throwError $ "bopSeq: bad"

bop :: (forall a . Ord a => a -> a -> Bool) -> Object -> Object -> SchemeM Object
bop op (OInt n) (OInt m) = return $ OBool $ n `op` m
bop _ _ _ = throwError $ "bop: bad "

bbop :: (Bool -> Bool -> Bool) -> Object -> Object -> SchemeM Object
bbop op (OBool b) (OBool c) = return $ OBool $ b `op` c
bbop _ _ _ = throwError $ "bbop: bad"

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
		_ -> return x
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

define :: Object -> SchemeM Object
define (OCons v@(OVar var) (OCons val ONil)) = do
	r <- eval val
	case r of
		OClosure Nothing eid as bd -> modify $
			second ((EVar var, OClosure (Just var) eid as bd) :)
		_ -> modify $ second ((EVar var, r) :)
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

load :: Object -> SchemeM Object
load (OCons (OString fp) ONil) = do
	src <- liftIO $ readFile fp
	case prsf src of
		Just os -> mapM_ eval os >> return (OBool True)
		_ -> throwError "*** ERROR: parse error"

load _ = throwError "*** ERROR: load: bad arguments"

topEnv :: Object -> SchemeM Object
topEnv ONil = OEnv topEID . snd <$> get
topEnv _ = throwError "*** ERROR: topEnv: bad arguments"

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

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = do
	b <- act
	if b then doWhile_ act else return ()

eval :: Object -> SchemeM Object
eval i@(OInt _) = return i
eval d@(ODouble _) = return d
eval s@(OString _) = return s
eval (OVar var) = do
	((_, h), _) <- get
	let lval = case h of
		Just (_, henv) -> lookup (EVar var) henv
		Nothing -> Nothing
	mval <- gets $ (lookup $ EVar var) . snd
	case (lval, mval) of
		(Just v, _) -> return v
		(_, Just val) -> return val
		(_, _) -> throwError $ "*** ERROR: unbound variable: " ++ var
eval o@(OCons f_ args_) = do
	f <- eval f_
	case f of
		OSubr _ s -> s =<< mapList eval args_
		OSyntax _ s -> s args_
		OClosure _ eid as bd -> do
			args <- mapList eval args_
			eid' <- nowEnv
			intoEnv eid
			r <- apply as args bd
			intoEnv eid'
			return $ lastList r
		_ -> throwError $ "*** ERROR: invalid application: " ++ showObj o
eval ONil = return ONil
eval o@(OBool _) = return o
eval o@(OSyntax _ _) = return o
eval _ = throwError "eval: not yet constructed"

apply :: Object -> Object -> Object -> SchemeM Object
apply vs as bd = do
	_ <- zipWithList def vs as
	mapList eval bd

def :: Object -> Object -> SchemeM Object
def v@(OVar var) val = do
	((maxID, Just (hid, henv)), tenv) <- get
	put ((maxID, Just (hid, (EVar var, val) : henv)), tenv)
	return v
def _ _ = throwError "def: bad"

zipWithList :: (Object -> Object -> SchemeM Object) ->
	Object -> Object -> SchemeM Object
zipWithList f (OCons a d) (OCons a' d') =
	OCons <$> (f a a') <*> zipWithList f d d'
zipWithList _ ONil _ = return ONil
zipWithList _ _ ONil = return ONil
zipWithList _ _ _ = throwError "zipWithList: bad"

lastList :: Object -> Object
lastList (OCons a ONil) = a
lastList (OCons _ d) = lastList d
lastList _ = error "lastList: bad"

mapList :: (Object -> SchemeM Object) -> Object -> SchemeM Object
mapList _ ONil = return ONil
mapList f (OCons a d) = OCons <$> f a <*> mapList f d
mapList _ _ = throwError "mapList: not list"

foldListl :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
foldListl _ o0 ONil = return o0
foldListl f o0 (OCons a d) = flip (foldListl f) d =<< f o0 a
foldListl _ _ _ = throwError "foldListl: not list"

prsf :: String -> Maybe [Object]
prsf src = case runError $ scmf $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

dpt :: String -> Maybe Int
dpt src = case runError $ depth' $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

prs :: String -> Maybe Object
prs src = case runError $ scm $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

data Object
	= OInt Integer
	| ORational Rational
	| ODouble Double
	| OString String
	| OVar String
	| OCons Object Object
	| ONil
	| OUndef
	| OBool Bool
	| OSubr String (Object -> SchemeM Object)
	| OSyntax String (Object -> SchemeM Object)
	| OEnv EID Env
	| OClosure (Maybe String) EID Object Object

instance Show Object where
	show (ODouble d) = "(ODouble " ++ show d ++ ")"
	show _ = "show Object: yet"

showObj :: Object -> String
showObj (OInt i) = show i
showObj (ORational r) = show (numerator r) ++ "/" ++ show (denominator r)
showObj (ODouble d) = show d
showObj (OString s) = show s
showObj (OVar v) = v
showObj (OSubr n _) = "#<subr " ++ n ++ ">"
showObj (OSyntax n _) = "#<syntax " ++ n ++ ">"
showObj (OCons (OVar "quote") (OCons a ONil)) = "'" ++ showObj a
showObj c@(OCons _ _) = showCons False c
showObj ONil = "()"
showObj (OBool True) = "#t"
showObj (OBool False) = "#f"
showObj OUndef = "#<undef>"
showObj (OEnv eid _) = "#<env " ++ show eid ++ ">"
showObj (OClosure n _ _ _) = "#<closure " ++ fromMaybe "#f" n ++ ">"

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
	| TStringL String
	| TVar String
	| TTrue
	| TFalse
	| TOParen
	| TCParen
	| TDot
	| TQuote

isVar :: Char -> Bool
isVar = (||) <$> isAlpha <*> (`elem` "+-*/<=>")

[papillon|

scmf :: [Object]
	= os:obj* _:spaces !_	{ os }

depth' :: Int
	= d:depth _:spaces !_	{ d }

depth :: Int
	= TOParen:lx _:obj* d:depth	{ d + 1 }
	/ TCParen:lx			{ - 1 }
	/				{ 0 }

scm :: Object
	= o:obj _:spaces !_	{ o }

obj :: Object
	= (TIntL i):lx		{ OInt i }
	/ (TDoubleL d):lx	{ ODouble d }
	/ (TStringL s):lx	{ OString s }
	/ (TVar v):lx		{ OVar v }
	/ TOParen:lx os:obj* TCParen:lx
				{ foldr OCons ONil os }
	/ TOParen:lx as:obj* TDot:lx d:obj TCParen:lx
				{ foldr OCons d as }
	/ TQuote:lx o:obj	{ OCons (OVar "quote") $ OCons o ONil}
	/ TTrue:lx		{ OBool True }
	/ TFalse:lx		{ OBool False }

lx :: Tkn
	= _:spaces w:word	{ w }

word :: Tkn
	= n:<isDigit>+ '.' d:<isDigit>+
				{ TDoubleL $ read $ n ++ "." ++ d }
	/ mm:('-' { '-' })? ds:<isDigit>+
				{ TIntL $ read $ maybe ds (: ds) mm }
	/ s:string		{ TStringL s }
	/ v:<isVar>+		{ TVar v }
	/ '('			{ TOParen }
	/ ')'			{ TCParen }
	/ '.'			{ TDot }
	/ '\''			{ TQuote }
	/ '#' 't'		{ TTrue }
	/ '#' 'f'		{ TFalse }

string :: String = '"' s:(<(`notElem` "\"\\")> / '\\' c:esc { c })* '"'
				{ s }

esc :: Char
	= 'n'			{ '\n' }

spaces = _:<isSpace>*

|]
