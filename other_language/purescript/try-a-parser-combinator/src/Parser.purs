module Parser where

import Prelude

import Control.Apply
import Control.Monad
import Control.Alt
import Control.Plus
import Control.Alternative
import Control.Lazy
import Data.Semigroup
import Data.Functor
import Data.Foldable
import Data.Function
import Data.Eq
import Data.Tuple
import Data.Maybe
import Data.List
import Data.Unit
import Data.Int
import Data.String.CodePoints as CP
import Data.String
import Data.String.CodeUnits
import Data.CodePoint.Unicode

newtype Parser a b = Parser (List a -> Maybe (Tuple b (List a)))

runParser :: forall a b . Parser a b -> List a -> Maybe (Tuple b (List a))
runParser (Parser g) = g

runParserStr ::
        forall b . Parser CodePoint b -> String -> Maybe (Tuple b (List CodePoint))
runParserStr p = runParser p <<< convertStringToListOfCodePoint

convertStringToListOfCodePoint :: String -> List CodePoint
convertStringToListOfCodePoint = fromFoldable <<< toCodePointArray

instance Functor (Parser a) where
        map g f = Parser \s -> first g <$> f `runParser` s

instance Apply (Parser a) where
        apply fg f = Parser \s -> do
                Tuple g s' <- fg `runParser` s
                (g `first` _) <$> f `runParser` s'

instance Applicative (Parser a) where pure x = Parser $ Just <<< Tuple x

instance Bind (Parser a) where
        bind m g =
                Parser \s -> uncurry runParser <<< first g =<< m `runParser` s

instance Monad (Parser a)

instance Alt (Parser a) where
        alt p1 p2 = Parser \s -> case p1 `runParser` s of
                Nothing -> p2 `runParser` s
                Just r -> Just r

instance Plus (Parser a) where
        empty = Parser $ const Nothing

instance Alternative (Parser a)

instance Lazy (Parser a (List b)) where
        defer g = Parser \s -> runParser (g unit) s

first :: forall a b c . (a -> b) -> Tuple a c  -> Tuple b c
first f (Tuple x y) = Tuple (f x) y

fail :: forall a b . Parser a b
fail = Parser $ const Nothing

anyToken :: forall a . Parser a a
anyToken = Parser \s -> case s of
        Nil -> Nothing
        x : xs -> Just $ Tuple x xs

sat :: forall a . (a -> Boolean) -> Parser a a
sat pred = do
        c <- anyToken
        if pred c then pure c else fail

digit :: Parser CodePoint CodePoint
digit = sat isDigit

lower :: Parser CodePoint CodePoint
lower = sat isLower

upper :: Parser CodePoint CodePoint
upper = sat isUpper

letter :: Parser CodePoint CodePoint
letter = sat isAlpha

alphanum :: Parser CodePoint CodePoint
alphanum = sat isAlphaNum

char :: Char -> Parser CodePoint CodePoint
char c = sat (_ == codePointFromChar c)

string :: String -> Parser CodePoint String
string s = s <$ stringGen s

stringGen :: String -> Parser CodePoint Unit
stringGen s = case CP.uncons s of
        Nothing -> pure unit
        Just { head : c, tail : s' } -> do
                _ <- sat (_ == c)
                stringGen s'

spaces :: Parser CodePoint Unit
spaces = do
        c <- anyToken
        if c == codePointFromChar ' '
                then spaces
                else Parser $ Just <<< Tuple unit <<< (c : _)

spaces' :: Unit -> Parser CodePoint Unit
spaces' _ = (char ' ' *> spaces' unit) <|> pure unit

spaces0 :: Parser CodePoint Unit
spaces0 = unit <$ many (char ' ')

spaces1 :: Parser CodePoint Unit
spaces1 = unit <$ some (char ' ')

fromCodePointList :: List CodePoint -> String
fromCodePointList = fromCodePointArray <<< toUnfoldable

int :: Parser CodePoint Int
int = neg <|> nat

nat :: Parser CodePoint Int
nat = do
        xs <- some digit
        case fromString $ fromCodePointList xs of
                Nothing -> empty
                Just n -> pure n

neg :: Parser CodePoint Int
neg = char '-' *> (negate <$> nat)
