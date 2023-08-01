{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MakeStruct where

import Data.List
import Data.Char
import Text.Nowdoc

vulkanCoreH :: FilePath
vulkanCoreH = "/usr/include/vulkan/vulkan_core.h"

directory :: String -> FilePath
directory mn = "../src/" ++ intercalate "/" (init $ sep '.' mn)

cName :: String -> String -> String
cName mn hsn = "Vk" ++ concat (tail . init $ sep '.' mn) ++ hsn

data Name = Atom String | List String Int deriving Show

showName :: Name -> String
showName = \case
	Atom nm -> "A " ++ nm
	List nm nb -> "L " ++ nm ++ " " ++ show nb

header :: String -> String
header mn = [nowdoc|
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module |] ++ mn ++ [nowdoc| where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base

#include <vulkan/vulkan.h>

|]

make :: String -> [String] -> IO ()
make moduleName hsNames = do
	vch <- readFile vulkanCoreH
	let	moduleName' = intercalate "." . init $ sep '.' moduleName
		dss = (\hn -> takeDefinition (cName moduleName' hn) $ lines vch) <$> hsNames
		hsNameDss = zip hsNames dss
	uncurry (makeThText moduleName') `mapM_` hsNameDss

makeThText :: String -> String -> [(String, Name)] -> IO ()
makeThText moduleName' hsName defs =
	writeFile ("../th/" ++ unCapitalize (cName moduleName' hsName) ++ ".txt")
		. unlines $ map (showWithBar id showName) defs

showWithBar :: (a -> String) -> (b -> String) -> (a, b) -> String
showWithBar sa sb (x, y) = sa x ++ "|" ++ sb y

field1 :: String -> String -> Name -> String
field1 csn t (Atom n) = "\t(\"" ++ n ++ "\", ''#{type " ++ t ++ "},\n\t\t[| #{peek " ++
	csn ++ ", " ++ n ++ "} |],\n\t\t[| #{poke " ++
	csn ++ ", " ++ n ++ "} |])"
field1 csn t (List nm nb) = "\t(\"" ++ nm ++ "\", ''" ++ listOf t ++ ",\n\t\t[| peekArray " ++
	show nb ++ " . #{ptr " ++ csn ++ ", " ++ nm ++ "}" ++ "|],\n\t\t[| pokeArray . #{ptr " ++
	csn ++ ", " ++ nm ++ "} |])"

takeDefinition :: String -> [String] -> [(String, Name)]
takeDefinition nm = map ((\[t, n] -> (t, makeName $ init n)) . words)
	. takeWhile (not . (== "} " ++ nm ++ ";")) . tail
	. dropWhile (not . (("typedef struct " ++ nm ++ " {") `isPrefixOf`))

makeName :: String -> Name
makeName str = case span (/= '[') str of
	(t, "") -> Atom t
	(t, '[' : n_) -> List t . read $ init n_
	_ -> error "bad list"

listOf :: String -> String
listOf t = "List" ++ (concat . map capitalize $ sep '_' t)

sep :: Eq a => a -> [a] -> [[a]]
sep s xs = case d of [] -> [t]; _ : r -> t : sep s r
	where (t, d) = span (/= s) xs

capitalize :: String -> String
capitalize = \case "" -> ""; c : cs -> toUpper c : map toLower cs

unCapitalize :: String -> String
unCapitalize = \case "" -> ""; c : cs -> toLower c : cs
