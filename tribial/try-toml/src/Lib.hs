{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Monad
import Data.Char
import Text.Parsec hiding (digit, newline)
import Numeric

-- Overall Structure

-- Whitespace

ws :: Parsec String () ()
ws = void $ many wschar

wschar :: Parsec String () Char
wschar = char '\x20' <|> char '\x09'

-- Newline

newline :: Parsec String () String
newline =
	(: []) <$> char '\x0A' <|>
	(\r n -> [r, n]) <$> char '\x0D' <*> char '\x0A'

-- Comment

commentStartSymbol :: Parsec String () ()
commentStartSymbol = void $ char '\x23'

nonAscii :: Parsec String () Char
nonAscii = oneOf $ ['\x80' .. '\xD7FF'] <> ['\xE000' .. '\x10FFFF']

nonEol :: Parsec String () Char
nonEol = char '\x09' <|> oneOf ['\x20' .. '\x7F'] <|> nonAscii

comment :: Parsec String () String
comment = commentStartSymbol >> many nonEol

-- Key-Value pairs

key :: Parsec String () [String]
key = (: []) <$> simpleKey <|> dottedKey

simpleKey :: Parsec String () String
simpleKey = quotedKey <|> unquotedKey

unquotedKey, quotedKey :: Parsec String () String
unquotedKey = many1 $ alpha <|> digit <|> char '\x2D' <|> char '\x5F'
quotedKey = basicString <|> literalString

dottedKey :: Parsec String () [String]
dottedKey = simpleKey >> many1 (dotSep >> simpleKey)

dotSep, keyvalSep :: Parsec String () ()
dotSep = void $ ws >> char '\x2E' >> ws
keyvalSep = void $ ws >> char '\x3D' >> ws

-- String

-- Basic String

basicString :: Parsec String () String
basicString = quotationMark >> many basicChar <* quotationMark

quotationMark :: Parsec String () ()
quotationMark = void $ char '\x22'

basicChar :: Parsec String () Char
basicChar = basicUnescaped <|> escaped

basicUnescaped :: Parsec String () Char
basicUnescaped = wschar <|> char '\x21' <|>
	oneOf (['\x23' .. '\x5B'] <> ['\x5D' .. '\x7E']) <|> nonAscii

escaped :: Parsec String () Char
escaped = escape >> escapeSeqChar

escape :: Parsec String () ()
escape = void $ char '\x5C'

escapeSeqChar :: Parsec String () Char
escapeSeqChar =
	char '\x22' <|> char '\x5C' <|>
	'\x08' <$ char '\x62' <|> '\x0C' <$ char '\x66' <|>
	'\x0A' <$ char '\x6E' <|> '\x0D' <$ char '\x72' <|>
	'\x09' <$ char '\x74' <|>
	chr . fst . head . readHex <$> (char '\x75' >> count 4 hexdig) <|>
	chr . fst . head . readHex <$> (char '\x55' >> count 8 hexdig)

-- Multiline Basic String

-- Literal String

literalString :: Parsec String () String
literalString = apostroph >> many literalChar <* apostroph

apostroph :: Parsec String () ()
apostroph = void $ char '\x27'

literalChar :: Parsec String () Char
literalChar =
	char '\x09' <|> oneOf (['\x20' .. '\x26'] <> ['\x28' .. '\x7E']) <|>
	nonAscii

-- Multiline Literal String

-- Integer

-- Float

-- Boolean

-- Date and Time (as defined in RFC 3339)

-- Offset Date-Time

-- Local Date-Time

-- Local Date

-- Local Time

-- Array

-- Table

-- Standard Table

-- Inline Table

-- Array Table

-- Build-in ABNF terms, reproduced here for clarity

alpha, digit, hexdig :: Parsec String () Char
alpha = oneOf $ ['\x41' .. '\x5A'] <> ['\x61' .. '\x7A']
digit = oneOf ['\x30' .. '\x39']
hexdig = digit <|> (oneOf $ ['A' .. 'F'] <> ['a' .. 'f'])

-- TYPES

data Value = String String
