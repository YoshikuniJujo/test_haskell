{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec

test8 :: Stream s m Char => ParsecT s u m [Char]
test8 = many (letter <|> digit)
