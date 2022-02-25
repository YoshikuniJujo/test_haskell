{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanPhysicalDeviceStruct where

import Data.List

vulkanCoreH :: FilePath
vulkanCoreH = "/usr/include/vulkan/vulkan_core.h"

make :: IO ()
make = do
	vch <- readFile vulkanCoreH
	let	ds = takeDefinition "VkPhysicalDeviceLimits" $ lines vch
	putStr . concat $ uncurry (field1 "VkPhysicalDeviceLimits") <$> ds

field1 :: String -> String -> String -> String
field1 csn t n = "\t(\"" ++ n ++ "\", ''#{type " ++ t ++ "},\n\t\t[| #{peek " ++
	csn ++ ", " ++ n ++ "} |],\n\t\t[| #{poke " ++
	csn ++ ", " ++ n ++ "} |]),\n"

takeDefinition :: String -> [String] -> [(String, String)]
takeDefinition nm = map ((\[t, n] -> (t, init n)) . words)
	. takeWhile (not . (== "} " ++ nm ++ ";")) . tail
	. dropWhile (not . (("typedef struct " ++ nm ++ " {") `isPrefixOf`))
