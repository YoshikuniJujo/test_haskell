module Parser where

import Data.Functor
import Data.Function
import Data.Tuple
import Data.Maybe
import Data.List

newtype Parser a b = Parser (List a -> Maybe (Tuple b (List a)))

runParser :: forall a b . Parser a b -> List a -> Maybe (Tuple b (List a))
runParser (Parser g) = g

instance Functor (Parser a) where
        map g f = Parser \s -> case runParser f s of
                Just (Tuple v s') -> Just $ Tuple (g v) s'
                Nothing -> Nothing
