{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryLex where

import Control.Exception
import Data.Loc

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import qualified Data.ByteString as BS
import qualified "language-c-quote" Language.C.Parser as P
import qualified "language-c-quote" Language.C.Parser.Tokens as T
import qualified "language-c-quote" Language.C.Syntax as C

lexFile :: [C.Extensions] -> FilePath -> IO ()
lexFile exts filename = do
	buf <- BS.readFile filename
	print buf
	case tokens buf of
		Left err -> fail $ show err
		Right ts -> mapM_ print ts
	where
	tokens :: BS.ByteString -> Either SomeException [L T.Token]
	tokens buf = P.evalP tokensP (P.emptyPState exts [] buf start)

	start :: Maybe Pos
	start = Just $ startPos filename

	tokensP :: P.P [L T.Token]
	tokensP = do
		t <- P.lexToken
		case t of
			L _ T.Teof -> return []
			_ -> tokensP >>= \ts -> return (t : ts)

parseFile :: [C.Extensions] -> String -> IO ()
parseFile exts filename = do
	s <- BS.readFile filename
	case P.parse exts [] P.parseUnit s start of
		Left err -> fail $ show err
		Right defs -> do
			putStr $ prettyPragma 80 (ppr defs)
			putStr $ pretty 80 (ppr defs)
			print defs
	where
	start :: Maybe Pos
	start = Just $ startPos filename
