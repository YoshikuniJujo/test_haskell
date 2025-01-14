{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Flip2 where

import Control.Monad
import Data.List
import Data.Curry
import Data.Swizzle qualified as Swz

flip13 :: (a -> b -> c -> r) -> c -> b -> a -> r
flip13 f = crr3 $ unc3 f . Swz.zyx

foo :: (Show a, Show b, Show c, Show d) => a -> b -> c -> d -> String
foo x y z w = show x ++ show y ++ show z ++ show w

bar :: String
bar = flip13 foo 1 2 3 4

baz :: IO ()
baz = crr3 (unc3 zipWithM_ . Swz.zxy) [1 :: Int ..] strs \i n ->
	putStrLn $ show i ++ " " ++ n
	where
	strs = ["Hello", "World", "Yoshio"]
