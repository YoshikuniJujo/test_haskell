module Main where

import Lecture

subtitle :: String
subtitle = "第16回 型族"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 型族(type family)について学ぶ", \t -> do
	text t "* 型族はHaskell 2010には取り込まれていない", \t -> do
	text t "* ghcの拡張機能", \t -> do
	text t "* ソースコードに{-# LANGUAGE TypeFamilies #-}をつける", \t -> do
	text t "* 次の標準には入るかも"
 ]
