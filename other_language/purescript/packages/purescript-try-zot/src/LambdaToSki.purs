module LambdaToSki where

import Prelude

import Control.Alt
import Data.Semigroup
import Data.Generic.Rep
import Data.Enum
import Data.Show
import Data.Show.Generic
import Data.Tuple
import Data.Maybe
import Data.List
import Data.Boolean
import Data.String
import Partial.Unsafe

import Parser

data Lambda
        = Var String | Apply Lambda Lambda | Fun String Lambda | S | K | I
        | Error String

instance Show Lambda where
        show S = "s"
        show K = "k"
        show I = "i"
        show (Apply f a) = "`" <> show f <> show a
        show (Fun p e) = "(fun " <> p <> " " <> show e <> ")"
        show (Var v) = "(var " <> v <> ")"
        show (Error msg) = "(error " <> msg <> ")"

onlySKI :: Lambda -> Boolean
onlySKI S = true
onlySKI K = true
onlySKI I = true
onlySKI (Apply f a) = onlySKI f && onlySKI a
onlySKI _ = false

makeFun :: List String -> Lambda -> Lambda
makeFun (p : Nil) ex = Fun p ex
makeFun (p : ps) ex = Fun p $ makeFun ps ex
makeFun _ _ = Error "makeFun: error: need 1 parameter at least"

lambdaToSki :: Lambda -> Lambda
lambdaToSki (Fun x e) = out x e
lambdaToSki (Apply f a) = Apply (lambdaToSki f) (lambdaToSki a)
lambdaToSki _ = Error "lambdaToSki: error"

out :: String -> Lambda -> Lambda
out _ e | onlySKI e = Apply K e
out x0 v@(Var x1)
        | x1 == x0 = I
        | otherwise = Apply K v
out x0 (Apply f a) = Apply (Apply S $ out x0 f) $ out x0 a
out x0 (Fun x1 e)
        | x0 == x1 = Apply K $ out x0 e
        | otherwise = out x0 $ out x1 e
out _ _ = Error "NEVER OCCUR"

readLambda' :: Partial => String -> Lambda
readLambda' s = case readLambda s of Just (Tuple r Nil) -> r

readLambda :: String -> Maybe (Tuple Lambda (List Token))
readLambda s = runParser (parseLambda unit) =<< lexer s

parseLambda :: Unit -> Parser Token Lambda
parseLambda _ = parseApply unit `alt` parseFun

parseApply :: Unit -> Parser Token Lambda
parseApply _ = recL1 Apply $ parseAtom unit

parseFun :: Parser Token Lambda
parseFun = do
        _ <- sat (_ == Lambda)
        ps <- parseParams
        _ <- sat (_ == RightArrow)
        e <- parseLambda unit
        pure $ makeFun ps e

parseAtom :: Unit -> Parser Token Lambda
parseAtom _ =
        (Var <<< unsafePartial varName <$> sat isVar)
        `alt`
        parseParens

parseParens :: Parser Token Lambda
parseParens = do
        _ <- sat (_ == OpenParen)
        r <- parseLambda unit
        _ <- sat (_ == CloseParen)
        pure r

parseParams :: Parser Token (List String)
parseParams = some $ unsafePartial varName <$> sat isVar

data Token = Lambda | RightArrow | OpenParen | CloseParen | VarT String

derive instance Generic Token _
derive instance Eq Token
instance Show Token where show = genericShow

isVar :: Token -> Boolean
isVar (VarT _) = true
isVar _ = false

varName :: Partial => Token -> String
varName (VarT nm) = nm

lexer :: String -> Maybe (List Token)
lexer = lexerCP <<< map fromEnum <<< fromFoldable <<< toCodePointArray

lexerCP :: List Int -> Maybe (List Token)
lexerCP Nil = Just Nil
lexerCP (0x20 : rest) = lexerCP rest                                    -- ' '
lexerCP (0x0a : rest) = lexerCP rest                                    -- '\n'
lexerCP (0x5c : rest) = (Lambda : _) <$> lexerCP rest                   -- '\\'
lexerCP (0x2d : 0x3e : rest) = (RightArrow : _) <$> lexerCP rest        -- '-' '>'
lexerCP (0x28 : rest) = (OpenParen : _) <$> lexerCP rest                -- '('
lexerCP (0x29 : rest) = (CloseParen : _) <$> lexerCP rest               -- ')'
lexerCP ca@(c : _) | 0x61 <= c && c <= 0x7a = let                       -- isLower
        { init : ret, rest : rest } = span isAlphaNum ca in
        (VarT (codePointListToString ret) : _) <$> lexerCP rest
lexerCP _ = Nothing

isAlphaNum :: Int -> Boolean
isAlphaNum c = 
        0x41 <= c && c <= 0x5a ||
        0x61 <= c && c <= 0x7a ||
        0x30 <= c && c <= 0x39

codePointListToString :: List Int -> String
codePointListToString = fromCodePointArray <<< toUnfoldable <<< mapMaybe toEnum

idLambda :: String
idLambda = "\\c -> \\x -> \\print -> print c"

revLambda :: String
revLambda =
        "( \\x -> x x ( \\x -> x ) ) ( \\self -> \\remainder -> \\c ->" <>
        "c ( \\x -> x ) ( \\x -> x ) ( \\x -> x ) ( \\x -> \\y -> x )" <>
        "( self self ) ( self self ) ( \\print -> remainder ( print c ) ) )"
