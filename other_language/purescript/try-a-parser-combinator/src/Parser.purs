module Parser where

import Control.Monad
import Control.Apply
import Data.Functor
import Data.Function
import Data.Tuple
import Data.Maybe
import Data.List

newtype Parser a b = Parser (List a -> Maybe (Tuple b (List a)))

runParser :: forall a b . Parser a b -> List a -> Maybe (Tuple b (List a))
runParser (Parser g) = g

instance Functor (Parser a) where
        map g f = Parser \s -> first g <$> f `runParser` s

instance Apply (Parser a) where
        apply fg f = Parser \s -> do
                Tuple g s' <- fg `runParser` s
                (g `first` _) <$> f `runParser` s'

first :: forall a b c . (a -> b) -> Tuple a c  -> Tuple b c
first f (Tuple x y) = Tuple (f x) y
