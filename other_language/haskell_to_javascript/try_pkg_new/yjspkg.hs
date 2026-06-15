{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment

import JSPackage.Compile
import JSPackage.Clean
import JSPackage.Archive
import JSPackage.Place
import JSPackage.MkConf
import JSPackage.Register
import JSPackage.Expose
import JSPackage.Hide
import JSPackage.Unregister
import JSPackage.Unplace

main :: IO ()
main = do
	(cmd, as) <- (<$> getArgs) \case
		dr : c : a -> (c, dr : a)
		c : a -> (c, a)
		_ -> error "bad"
	case lookup cmd cmds of
		Nothing -> error "no such command"
		Just f -> f as

cmds :: [(String, [String] -> IO ())]
cmds = [
	("compile", compile),
	("archive", archive),
	("mkConf", mkConf),
	("clean", clean),

	("place", place),
	("register", register),
	("expose", expose),
	("hide", hide),
	("unregister", unregister),
	("unplace", unplace),

	("build", \as ->
		compile as >> archive as >> mkConf as),
	("install", \as ->
		place as >> register as >> expose as),
	("uninstall", \as ->
		unregister as >> unplace as) ]
