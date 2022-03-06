{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MakeEnum where

import Data.List
import Data.Char
import Text.Nowdoc
import System.Environment
import System.Directory
import System.FilePath

type HeaderFile = FilePath
type ModuleName = String
type IncludeModule = String
type HaskellName = String
type CName = String
type ExtraCode = String

type HeaderCode = String
type HaskellCode = String

type DerivName = String

createSingleFile :: HeaderFile -> ModuleName -> HaskellName -> CName -> ExtraCode -> IO ()
createSingleFile hf mnm hsnm snm ext = do
	prg <- getProgName
	src <- readFile hf
	writeFile ("../src/Vulkan/" ++ substitute '.' '/' mnm ++ ".hsc") $
		header prg [] mnm ++
		makeEnum src (hsnm, snm, ["Show", "Eq", "Storable"]) ++
		case ext of "" -> ""; _ -> "\n" ++ ext ++ "\n"

substitute :: Char -> Char -> String -> String
substitute x y = map (\c -> if c == x then y else c)

createFile :: HeaderFile -> ModuleName -> [(HaskellName, CName)] -> IO ()
createFile hf mnm hscnms = do
	prg <- getProgName
	src <- readFile hf
	writeFile ("../src/Vulkan/" ++ substitute '.' '/' mnm ++ ".hsc") $
		header prg [] mnm ++ intercalate "\n" (map (makeEnum' src mnm []) hscnmdrvs)
	where hscnmdrvs = (`snocTuple2` ["Show", "Eq", "Storable"]) <$> hscnms

createFile' ::
	HeaderFile -> ModuleName -> [IncludeModule] ->
	[(HaskellName, CName, [DerivName])] -> ExtraCode -> IO ()
createFile' hf mnm icms hscnmdrvs ext = do
	prg <- getProgName
	src <- readFile hf
	createDirectoryIfMissing True . takeDirectory $ "../src/Vulkan/" ++ substitute '.' '/' mnm
	writeFile ("../src/Vulkan/" ++ substitute '.' '/' mnm ++ ".hsc") $
		header prg icms mnm ++ intercalate "\n" (map (makeEnum' src mnm []) hscnmdrvs) ++
		case ext of "" -> ""; _ -> "\n" ++ ext ++ "\n"

createFile'' ::
	HeaderFile -> ModuleName -> [IncludeModule] ->
	[([(String, Const)], (HaskellName, CName, [DerivName]))] -> ExtraCode -> IO ()
createFile'' hf mnm icms hscnmdrvs ext = do
	prg <- getProgName
	src <- readFile hf
	createDirectoryIfMissing True . takeDirectory $ "../src/Vulkan/" ++ substitute '.' '/' mnm
	writeFile ("../src/Vulkan/" ++ substitute '.' '/' mnm ++ ".hsc") $
		header prg icms mnm ++ intercalate "\n" (map (uncurry $ makeEnum' src mnm) hscnmdrvs) ++
		case ext of "" -> ""; _ -> "\n" ++ ext ++ "\n"

snocTuple2 :: (a, b) -> c -> (a, b, c)
snocTuple2 (x, y) z = (x, y, z)

makeEnum :: HeaderCode -> (HaskellName, CName, [DerivName]) -> HaskellCode
makeEnum src (hsnm, cnm, drvs) = body hsnm cnm drvs ++
	intercalate ",\n"
		(map makeItem . takeDefinition cnm
			. removeBetaExtensions $ lines src ) ++
		" ]\n"

makeEnum' :: HeaderCode -> ModuleName -> [(String, Const)] -> (HaskellName, CName, [DerivName]) -> HaskellCode
makeEnum' src mnm elms (hsnm, cnm, drvs) = body hsnm cnm drvs ++
	intercalate ",\n"
		(map (makeItem' mnm) . (elms ++) . map makeVarConstPair . takeDefinition cnm
			. removeBetaExtensions $ lines src ) ++
		" ]\n"

modNameToRemStr :: ModuleName -> (String, String)
modNameToRemStr mnm = (filter (/= '.') $ removeTail ".Enum" mnm', rmt)
	where (mnm', rmt) = case spanWith '.' mnm of
		Right ("Ext", rst) -> (rst, "Ext")
		Right ("Khr", rst) -> (rst, "Khr")
		_ -> (mnm, "")

spanWith :: Eq a => a -> [a] -> Either [a] ([a], [a])
spanWith x xs = case span (/= x) xs of
	(pre, _ : pst) -> Right (pre, pst)
	_ -> Left xs

removeTail :: String -> String -> String
removeTail rm str
	| rm `isSuffixOf` str = take ln str
	| otherwise = str
	where ln = length str - length rm

removeInit :: String -> String -> String
removeInit rm str
	| rm `isPrefixOf` str = drop ln str
	| otherwise = str
	where ln = length rm

removeBetaExtensions :: [String] -> [String]
removeBetaExtensions [] = []
removeBetaExtensions ("#ifdef VK_ENABLE_BETA_EXTENSIONS" : ls) =
	case dropWhile (/= "#endif") ls of
		[] -> error "no #endif"
		(_ : ls') -> removeBetaExtensions ls'
removeBetaExtensions (l : ls) = l : removeBetaExtensions ls

{-
makeEnum' :: String -> [String] -> String -> String -> [String] -> String -> IO ()
makeEnum' hf icds hsnm cnm drvs ext = do
	prg <- getProgName
	src <- readFile hf
	writeFile ("../src/Vulkan/" ++ hsnm ++ ".hsc") $
		header prg icds hsnm ++ body hsnm cnm drvs ++
			intercalate ",\n" (map makeItem . takeDefinition cnm $ lines src) ++ " ]\n" ++
		case ext of "" -> ""; _ -> "\n" ++ ext ++ "\n"
-}

makeEnum'' :: String -> [String] -> String -> String -> [(String, Const)] -> [String] -> String -> IO ()
makeEnum'' hf icds hsnm cnm elms drvs ext = do
	prg <- getProgName
	src <- readFile hf
	writeFile ("../src/Vulkan/" ++ hsnm ++ ".hsc") $
		header prg icds hsnm ++ body hsnm cnm drvs ++
			intercalate ",\n" (
				map (uncurry $ makeItemFromConst ("", ""))
					. (elms ++)
					. map makeVarConstPair
					. takeDefinition cnm $ lines src
				) ++ " ]\n" ++
		case ext of "" -> ""; _ -> "\n" ++ ext ++ "\n"

takeDefinition :: String -> [String] -> [String]
takeDefinition nm =
	map (head . words) . takeWhile (not . (== "} " ++ nm ++ ";")) . tail'
		. dropWhile (not . (("typedef enum " ++ nm ++ " {") `isPrefixOf`))

data Const = Int Int | Const String deriving Show

makeVarConstPair :: String -> (String, Const)
makeVarConstPair cnst = (nm, Const cnst)
	where nm = concat . map capitalize . tail $ sep '_' cnst

tail' :: [a] -> [a]
tail' [] = error "bad"
tail' xs = tail xs

showConst :: Const -> String
showConst (Int n) = show n
showConst (Const cnst) = "#{const " ++ cnst ++ "}"

makeItemFromConst :: (String, String) -> String -> Const -> String
makeItemFromConst (rm, rmt) nm cnst = "\t(\"" ++ removeInit rm (removeTail rmt nm) ++ "\"," ++ sp ++ cst ++ ")"
	where
	cst = showConst cnst
	sp = if 8 + length nm + 5 + length cst + 2 > 80 then "\n\t\t" else " "

makeItem :: String -> String
makeItem = uncurry (makeItemFromConst ("", "")) . makeVarConstPair

makeItem' :: ModuleName -> (String, Const) -> String
makeItem' mnm =
	uncurry (makeItemFromConst $ modNameToRemStr mnm)

{-
makeItem cnst = '\t' : nm ++ sp ++ cst
	where
	nm = "(\"" ++ concat (map capitalize . tail $ sep '_' cnst) ++ "\","
	cst = "#{const " ++ cnst ++ "})"
	sp = if 8 + length nm + 1 + length cst + 1 > 80 then "\n\t\t" else " "
	-}

sep :: Eq a => a -> [a] -> [[a]]
sep s xs = case d of [] -> [t]; _ : r -> t : sep s r
	where (t, d) = span (/= s) xs
	
capitalize :: String -> String
capitalize = \case
	"" -> ""
	c : cs -> toUpper c : map toLower cs

header :: String -> [String] -> String -> String
header tnm icds mnm =
	"-- This file is automatically generated by the tools/" ++ tnm ++
	".hs" ++ "\n--\t% stack runghc --cwd tools/ " ++ tnm ++
	[nowdoc|


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module |] ++ "Vulkan." ++ mnm ++ [nowdoc| where

import Foreign.Storable
import Foreign.C.Enum
|] ++ unlines (("import " ++) <$> icds) ++ [nowdoc|

#include <vulkan/vulkan.h>

|]

body :: String -> String -> [String] -> String
body hsnm cnm drvs = "enum \"" ++ hsnm ++ "\" ''#{type " ++ cnm ++
	"}\n\t\t" ++ makeDrvs drvs ++ " [\n"

makeDrvs :: [String] -> String
makeDrvs = ('[' :) . (++ "]") . intercalate ", " . map ("''" ++)
