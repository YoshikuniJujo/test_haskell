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

vulkanCore :: HeaderFile
vulkanCore = "/usr/include/vulkan/vulkan_core.h"

substitute :: Char -> Char -> String -> String
substitute x y = map (\c -> if c == x then y else c)

createFile' ::
	HeaderFile -> ModuleName -> [IncludeModule] ->
	[(HaskellName, CName, [DerivName])] -> ExtraCode -> IO ()
createFile' hf mnm icms hscnmdrvs ext = do
	prg <- getProgName
	src <- readFile hf
	createDirectoryIfMissing True . takeDirectory $ "../src/Gpu/Vulkan/" ++ substitute '.' '/' mnm
	writeFile ("../src/Gpu/Vulkan/" ++ substitute '.' '/' mnm ++ ".hsc") $
		header prg icms mnm ++ intercalate "\n" (map (makeEnum' src mnm []) hscnmdrvs) ++
		case ext of "" -> ""; _ -> "\n" ++ ext ++ "\n"

createFile'' ::
	HeaderFile -> ModuleName -> [IncludeModule] ->
	[([(String, Const)], (HaskellName, CName, [DerivName]))] -> ExtraCode -> IO ()
createFile'' hf mnm icms hscnmdrvs ext = do
	prg <- getProgName
	src <- readFile hf
	createDirectoryIfMissing True . takeDirectory $ "../src/Gpu/Vulkan/" ++ substitute '.' '/' mnm
	writeFile ("../src/Gpu/Vulkan/" ++ substitute '.' '/' mnm ++ ".hsc") $
		header prg icms mnm ++ intercalate "\n" (map (uncurry $ makeEnum' src mnm) hscnmdrvs) ++
		case ext of "" -> ""; _ -> "\n" ++ ext ++ "\n"

createFileWithDefault ::
	HeaderFile -> ModuleName -> [IncludeModule] ->
	[(Maybe String, [(String, Const)], (HaskellName, CName, [DerivName]))] ->
	ExtraCode -> IO ()
createFileWithDefault hf mnm icms dfhscnmdrvs ext = do
	let	(mdf, hscnmdrvs) = unzip $ popTuple <$> dfhscnmdrvs
	prg <- getProgName
	src <- readFile hf
	createDirectoryIfMissing True . takeDirectory $ "../src/Gpu/Vulkan/" ++ substitute '.' '/' mnm
	writeFile ("../src/Gpu/Vulkan/" ++ substitute '.' '/' mnm ++ ".hsc") $
		header prg (icms ++ ["Data.Default"]) mnm ++
		intercalate "\n" (map (\((a, b), c) -> makeEnumWithDefault src mnm a b c) (zip hscnmdrvs mdf)) ++
		case ext of "" -> ""; _ -> "\n" ++ ext ++ "\n"

createRaw :: HeaderFile -> [String] -> CName -> IO ()
createRaw hf zrs cn@(c : cs) = do
	src <- readFile hf
	let	fp = "../th/" ++ toLower c : cs ++ ".txt"
		rslt = unlines $ zrs ++ ((fst . makeVarConstPair)
			<$> takeDefinition cn (lines src))
	writeFile fp rslt
createRaw _ _ _ = error "bad"

popTuple :: (a, b, c) -> (a, (b, c))
popTuple (x, y, z) = (x, (y, z))

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
		(map (makeItem' mnm) . (elms ++) . removeDups [] . map makeVarConstPair . takeDefinition cnm
			. removeBetaExtensions $ lines src ) ++
		" ]\n"

removeDups :: [String] -> [(String, Const)] -> [(String, Const)]
removeDups _ [] = []
removeDups os ((helm, celm) : ss) = if helm `elem` os
	then removeDups os ss
	else (helm, celm) : removeDups (helm : os) ss

makeEnumWithDefault ::
	HeaderCode -> ModuleName -> [(String, Const)] ->
	(HaskellName, CName, [DerivName]) -> Maybe String -> HaskellCode
makeEnumWithDefault src mnm elms hsnmcnmdrvs@(hsnm, _, _) mdf =
	makeEnum' src mnm elms hsnmcnmdrvs ++
	maybe "" (\df -> "\n" ++ instanceDefault hsnm df) mdf

instanceDefault :: HaskellName -> String -> HaskellCode
instanceDefault hsnm df = "instance Default " ++ hsnm ++ " where\n\tdef = " ++ df ++ "\n"

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

makeEnum'' :: String -> [String] -> String -> String -> [(String, Const)] -> [String] -> String -> IO ()
makeEnum'' hf icds hsnm cnm elms drvs ext = do
	prg <- getProgName
	src <- readFile hf
	writeFile ("../src/Vulkan/" ++ hsnm ++ ".hsc") $
		header prg icds hsnm ++ body hsnm cnm drvs ++
			intercalate ",\n" (
				map (uncurry $ makeItemFromConst ("", ""))
					. (elms ++)
					. removeDups []
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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-export-lists -fno-warn-tabs #-}

module |] ++ "Gpu.Vulkan." ++ mnm ++ [nowdoc| where

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
