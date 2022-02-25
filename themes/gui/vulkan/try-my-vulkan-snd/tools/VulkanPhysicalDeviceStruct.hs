{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanPhysicalDeviceStruct where

import Data.List
import Data.Char

vulkanCoreH :: FilePath
vulkanCoreH = "/usr/include/vulkan/vulkan_core.h"

data Name = Atom String | List String Int deriving Show

make :: IO ()
make = do
	vch <- readFile vulkanCoreH
	let	ds = takeDefinition "VkPhysicalDeviceLimits" $ lines vch
	putStr . intercalate ",\n" $ uncurry (field1 "VkPhysicalDeviceLimits") <$> ds

field1 :: String -> String -> Name -> String
field1 csn t (Atom n) = "\t(\"" ++ n ++ "\", ''#{type " ++ t ++ "},\n\t\t[| #{peek " ++
	csn ++ ", " ++ n ++ "} |],\n\t\t[| #{poke " ++
	csn ++ ", " ++ n ++ "} |])"
field1 csn t (List nm nb) = "\t(\"" ++ nm ++ "\", ''#{type " ++ listOf t ++ "},\n\t\t[| peekArray " ++
	show nb ++ " . #{ptr " ++ csn ++ ", " ++ nm ++ "}" ++ "|],\n\t\t[| pokeArray . #{ptr " ++
	csn ++ ", " ++ nm ++ "} |])"

takeDefinition :: String -> [String] -> [(String, Name)]
takeDefinition nm = map ((\[t, n] -> (t, makeName $ init n)) . words)
	. takeWhile (not . (== "} " ++ nm ++ ";")) . tail
	. dropWhile (not . (("typedef struct " ++ nm ++ " {") `isPrefixOf`))

makeName :: String -> Name
makeName str = case span (/= '[') str of
	(t, "") -> Atom (t ++ "foo")
	(t, '[' : n_) -> List t . read $ init n_
	_ -> error "bad list"

listOf :: String -> String
listOf t = "List" ++ (concat . map capitalize $ sep '_' t)

sep :: Eq a => a -> [a] -> [[a]]
sep s xs = case d of [] -> [t]; _ : r -> t : sep s r
	where (t, d) = span (/= s) xs

capitalize :: String -> String
capitalize = \case
	"" -> ""
	c : cs -> toUpper c : map toLower cs
