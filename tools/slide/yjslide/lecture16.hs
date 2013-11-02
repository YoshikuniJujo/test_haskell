module Main where

import Lecture

subtitle :: String
subtitle = "第16回 型族"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, classify
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 型族(type families)について学ぶ", \t -> do
	text t "* 型族はHaskell 2010には取り込まれていない", \t -> do
	text t "* ghcの拡張機能", \t -> do
	text t "* ソースコードに{-# LANGUAGE TypeFamilies #-}をつける"
 ]

classify :: Page
classify = [\t -> do
	writeTopTitle t "分類"
	text t "", \t -> do
	text t "* 型族(type families)には以下の2つがある", \t -> do
	itext t 1 "- data families", \t -> do
	itext t 1 "- type synonym families"
 ]
