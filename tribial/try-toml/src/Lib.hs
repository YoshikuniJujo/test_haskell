{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Monad
import Data.Maybe
import Data.Char
import Data.Time qualified as T
import Text.Parsec hiding (string, digit, newline)
import Numeric

parseToml :: String -> Either ParseError [Either ([String], Value) Table]
parseToml = parse toml ""

-- Overall Structure

toml :: Parsec String () [Either ([String], Value) Table]
toml = maybe id (:) <$> expression <*> (catMaybes <$> many (newline >> expression)) <* notFollowedBy (newline >> expression)

expression :: Parsec String () (Maybe (Either ([String], Value) Table))
expression =
	try (Nothing <$ (ws >> notFollowedBy keyval >> notFollowedBy table >> optional comment)) <|>
	try (Just . Left <$> (ws >> keyval <* ws <* optional comment)) <|>
	(Just . Right <$> (ws >> table <* ws <* optional comment))

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

keyval :: Parsec String () ([String], Value)
keyval = (,) <$> key <* keyvalSep <*> val

key :: Parsec String () [String]
key = (: []) <$> try (simpleKey <* notFollowedBy dotSep) <|> dottedKey

simpleKey :: Parsec String () String
simpleKey = quotedKey <|> unquotedKey

unquotedKey, quotedKey :: Parsec String () String
unquotedKey = many1 $ alpha <|> digit <|> char '\x2D' <|> char '\x5F'
quotedKey = basicString <|> literalString

dottedKey :: Parsec String () [String]
dottedKey = (:) <$> simpleKey <*> many1 (dotSep >> simpleKey)

dotSep, keyvalSep :: Parsec String () ()
dotSep = void $ ws >> char '\x2E' >> ws
keyvalSep = void $ ws >> char '\x3D' >> ws

val :: Parsec String () Value
val =	String <$> string <|>
	{-
	Boolean <$> bool <|>
	array <|>
	...
	-}
	DateTime <$> dateTime

-- String

string :: Parsec String () String
string = try mlBasicString <|> basicString <|> try mlLiteralString <|> literalString

-- Basic String

basicString :: Parsec String () String
basicString = quotationMark >> many basicChar <* quotationMark

quotationMark :: Parsec String () Char
quotationMark = char '\x22'

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

mlBasicString :: Parsec String () String
mlBasicString = do
	mlBasicStringDelim
	optional newline
	mlbBasicBody <* mlBasicStringDelim

mlBasicStringDelim :: Parsec String () ()
mlBasicStringDelim = void $ count 3 quotationMark

mlbBasicBody :: Parsec String () String
mlbBasicBody = (\cs css mq -> concat cs ++ concat css ++ fromMaybe [] mq)
	<$> many mlbContent
	<*> many (try ((++) <$> mlbQuotes <*> (concat <$> many1 mlbContent)))
	<*> optionMaybe (try mlbQuotes)

mlbContent :: Parsec String () String
mlbContent = (: []) <$> mlbChar <|> newline <|> [] <$ mlbEscapedNl

mlbChar :: Parsec String () Char
mlbChar = mlbUnescaped <|> try escaped

mlbQuotes :: Parsec String () String
mlbQuotes = (\q mq -> maybe [q] ((q :) . (: [])) mq)
	<$> quotationMark
	<*> (optionMaybe quotationMark <* notFollowedBy quotationMark)

mlbUnescaped :: Parsec String () Char
mlbUnescaped =
	wschar <|> char '\x21' <|> oneOf ['\x23' .. '\x5B'] <|>
	oneOf ['\x5D' .. '\x7E'] <|> nonAscii

mlbEscapedNl :: Parsec String () ()
mlbEscapedNl =
	void $ escape >> ws >> newline >> many ((: []) <$> wschar <|> newline)

-- Literal String

literalString :: Parsec String () String
literalString = apostrophe >> many literalChar <* apostrophe

apostrophe :: Parsec String () Char
apostrophe = char '\x27'

literalChar :: Parsec String () Char
literalChar =
	char '\x09' <|> oneOf (['\x20' .. '\x26'] <> ['\x28' .. '\x7E']) <|>
	nonAscii

-- Multiline Literal String

mlLiteralString :: Parsec String () String
mlLiteralString = do
	mlLiteralStringDelim
	optional newline
	bd <- mlLiteralBody
	mlLiteralStringDelim
	pure bd

mlLiteralStringDelim :: Parsec String () ()
mlLiteralStringDelim = void $ count 3 apostrophe

mlLiteralBody :: Parsec String () String
mlLiteralBody = (\cs css mq -> concat cs ++ concat css ++ fromMaybe "" mq)
	<$> many mllContent
	<*> many (try ((++) <$> mllQuotes <*> (concat <$> many1 mllContent)))
	<*> optionMaybe (try mllQuotes)

mllContent :: Parsec String () String
mllContent = (: "") <$> mllChar <|> newline

mllChar :: Parsec String () Char
mllChar = char '\09' <|>
	oneOf (['\x20' .. '\x26'] <> ['\x28' .. '\x7E']) <|> nonAscii

mllQuotes :: Parsec String () String
mllQuotes = (\q mq -> maybe [q] ((q :) . (: "")) mq)
	<$> apostrophe
	<*> (optionMaybe apostrophe <* notFollowedBy apostrophe)

-- Integer

-- Float

-- Boolean

-- Date and Time (as defined in RFC 3339)

dateTime :: Parsec String () DateTime
dateTime =
	OffsetDateTime <$> try offsetDateTime <|>
	LocalDateTime <$> try localDateTime <|>
	LocalDate <$> try localDate <|>
	LocalTime <$> localTime

dateFullyear :: Parsec String () T.Year
dateFullyear = read <$> count 4 digit

dateMonth :: Parsec String () T.MonthOfYear
dateMonth = read <$> count 2 digit

dateMday :: Parsec String () T.DayOfMonth
dateMday = read <$> count 2 digit

timeDelim :: Parsec String () ()
timeDelim = void $ char 'T' <|> char '\x20'

timeHour :: Parsec String () Int
timeHour = read <$> count 2 digit

timeMinute :: Parsec String () Int
timeMinute = read <$> count 2 digit

timeSecond :: Parsec String () String
timeSecond = count 2 digit

timeSecfrac :: Parsec String () String
timeSecfrac = (:) <$> char '.' <*> many1 digit

timeNumoffset :: Parsec String () T.TimeZone
timeNumoffset = hourMinutesToTimeZone
	<$> (char '+' <|> char '-') <*> timeHour <* char ':' <*> timeMinute

hourMinutesToTimeZone :: Char -> Int -> Int -> T.TimeZone
hourMinutesToTimeZone '+' h m = T.minutesToTimeZone (60 * h + m)
hourMinutesToTimeZone '-' h m = T.minutesToTimeZone (- (60 * h + m))
hourMinutesToTimeZone _ _ _ = error "+ / -"

timeOffset :: Parsec String () T.TimeZone
timeOffset = T.utc <$ char 'Z' <|> timeNumoffset

partialTime :: Parsec String () T.TimeOfDay
partialTime = T.TimeOfDay
	<$> timeHour <* char ':'
	<*> timeMinute <* char ':'
	<*> ((\i mf -> read $ maybe id (flip (<>)) mf i)
		<$> timeSecond <*> optionMaybe timeSecfrac)

fullDate :: Parsec String () T.Day
fullDate = T.fromGregorian
	<$> dateFullyear <* char '-' <*> dateMonth <* char '-' <*> dateMday

fullTime :: Parsec String () (T.TimeOfDay, T.TimeZone)
fullTime = (,) <$> partialTime <*> timeOffset

-- Offset Date-Time

offsetDateTime :: Parsec String () T.ZonedTime
offsetDateTime = (\d (td, tz) -> T.ZonedTime (T.LocalTime d td) tz)
	<$> fullDate <* timeDelim <*> fullTime

-- Local Date-Time

localDateTime :: Parsec String () T.LocalTime
localDateTime = T.LocalTime <$> fullDate <* timeDelim <*> partialTime

-- Local Date

localDate :: Parsec String () T.Day
localDate = fullDate

-- Local Time

localTime :: Parsec String () T.TimeOfDay
localTime = partialTime

-- Array

-- Table

table :: Parsec String () Table
table = try stdTable <|> arrayTable

-- Standard Table

stdTable :: Parsec String () Table
stdTable = StandardTable <$> (stdTableOpen >> key <* stdTableClose)

stdTableOpen, stdTableClose :: Parsec String () ()
stdTableOpen = char '\x5B' >> notFollowedBy (char '\x5B') >> ws
stdTableClose = void $ ws >> char '\x5D' >> notFollowedBy (char '\x5D')

-- Inline Table

-- Array Table

arrayTable :: Parsec String () Table
arrayTable = ArrayTable <$> (arrayTableOpen >> key <* arrayTableClose)

arrayTableOpen, arrayTableClose :: Parsec String () ()
arrayTableOpen = char '\x5B' >> char '\x5B' >> ws
arrayTableClose = void $ ws >> char '\x5D' >> char '\x5D'

-- Build-in ABNF terms, reproduced here for clarity

alpha, digit, hexdig :: Parsec String () Char
alpha = oneOf $ ['\x41' .. '\x5A'] <> ['\x61' .. '\x7A']
digit = oneOf ['\x30' .. '\x39']
hexdig = digit <|> (oneOf $ ['A' .. 'F'] <> ['a' .. 'f'])

-- TYPES

data Value
	= String String
	| DateTime DateTime
	deriving Show

data DateTime
	= OffsetDateTime T.ZonedTime | LocalDateTime T.LocalTime
	| LocalDate T.Day | LocalTime T.TimeOfDay
	deriving Show

data Table = StandardTable [String] | ArrayTable [String] deriving Show
