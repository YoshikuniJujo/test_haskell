import Data.Char

toCode :: Functor f => f Char -> f Int
toCode = fmap ord
