{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Color where

import GHC.Generics

import qualified Data.Swizzle.Class as S
import Data.Swizzle.TH
import Data.Curry

swizzle "yzwx"

newtype Red = Red Double deriving Show
newtype Green = Green Double deriving Show
newtype Blue = Blue Double deriving Show
newtype Alpha = Alpha Double deriving Show

data Argb = Argb {
	argbAlpha :: Alpha,
	argbRed :: Red,
	argbGreen :: Green,
	argbBlue :: Blue }
	deriving (Show, Generic)

instance S.Swizzle1 Argb where type X Argb = Alpha
instance S.Swizzle2 Argb where type Y Argb = Red
instance S.Swizzle3 Argb where type Z Argb = Green
instance S.Swizzle4 Argb where type W Argb = Blue

sampleArgb :: Argb
sampleArgb = Argb (Alpha 0.5) (Red 0.9) (Green 0.3) (Blue 0.2)

printRgba :: Red -> Green -> Blue -> Alpha -> IO ()
printRgba (Red r) (Green g) (Blue b) (Alpha a) = do
	putStrLn $ "red  : " ++ show r
	putStrLn $ "green: " ++ show g
	putStrLn $ "blue : " ++ show b
	putStrLn $ "alpha: " ++ show a

printArgb :: (
	S.Swizzle4 argb,
	S.X argb ~ Alpha, S.Y argb ~ Red, S.Z argb ~ Green, S.W argb ~ Blue ) =>
	argb -> IO ()
printArgb = unc4 printRgba . yzwx
