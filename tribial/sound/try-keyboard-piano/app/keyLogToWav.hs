{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.UI.GLFW qualified as Glfw
import System.Environment

import KeyLog
import WriteWave
import Waveform
import Hz qualified as Hz

main :: IO ()
main = do
	klfp : wvfp : _ <- getArgs
	kl <- readKeyLog klfp
	putMonoral16 wvfp
		. foldr addHorizontal (monoral 0 0)
--		$ keyLogToMonoral <$> filter ((== Glfw.KeyState'Pressed) . klAction) kl
--		$ uncurry keyLogPairToMonoral <$> pairs kl
		$ uncurry3 keyLogPairToMonoral' <$> pairs' kl

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : y : xs) = (x, y) : pairs xs
pairs _ = error "bad"

pairs' :: [a] -> [((a, a, a))]
pairs' [] = []
pairs' [x, y] = [(x, y, y)]
pairs' (x : y : xs@(z : _)) = (x, y, z) : pairs' xs

keyLogToMonoral :: KeyLog -> Monoral16
keyLogToMonoral kl = case klKey kl of
	Glfw.Key'D -> monoral Hz.doo 0.5
	Glfw.Key'F -> monoral Hz.re 0.5
	Glfw.Key'G -> monoral Hz.mi 0.5
	Glfw.Key'H -> monoral Hz.fa 0.5
	Glfw.Key'J -> monoral Hz.so 0.5
	Glfw.Key'K -> monoral Hz.la 0.5

keyLogPairToMonoral :: KeyLog -> KeyLog -> Monoral16
keyLogPairToMonoral pr rl = let
	hz = case klKey pr of
		Glfw.Key'D -> Hz.doo
		Glfw.Key'F -> Hz.re
		Glfw.Key'G -> Hz.mi
		Glfw.Key'H -> Hz.fa
		Glfw.Key'J -> Hz.so
		Glfw.Key'K -> Hz.la in
	monoral hz (klTime rl - klTime pr)

keyLogPairToMonoral' :: KeyLog -> KeyLog -> KeyLog -> Monoral16
keyLogPairToMonoral' pr rl nxt = let
	hz = case klKey pr of
		Glfw.Key'D -> Hz.doo
		Glfw.Key'F -> Hz.re
		Glfw.Key'G -> Hz.mi
		Glfw.Key'H -> Hz.fa
		Glfw.Key'J -> Hz.so
		Glfw.Key'K -> Hz.la
		Glfw.Key'L -> Hz.ti
		Glfw.Key'Semicolon -> Hz.hdo in
	monoral hz (klTime rl - klTime pr) `addHorizontal`
	monoral Hz.mu (klTime nxt - klTime rl)

uncurry3 f (a, b, c) = f a b c
