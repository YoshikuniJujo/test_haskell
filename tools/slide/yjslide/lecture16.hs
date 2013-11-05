module Main where

import Lecture

subtitle :: String
subtitle = "第16回 型族"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, whats, classify,
	dataFamilies, identity, identityFamily
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

whats :: Page
whats = [\t -> do
	writeTopTitle t "型族とは?"
	text t "", \t -> do
	text t "* Maybeについて見てみよう", \t -> do
	itext t 1 "data Maybe a = Just a | Nothing", \t -> do
	itext t 1 "- Maybeはすべての型aに対して同じ構造を取る", \t -> do
	text t "* Hoge IntとHoge Charとで違う構造の型を作りたい", \t -> do
	itext t 1 "- 今までの枠組では不可能", \t -> do
	itext t 1 "- Hoge aはaごとに違う定義を取らなければならない", \t -> do
	text t "* 中身の構造の違うデータ型や型シノニムをひとまとまりに", \t -> do
	arrowIText t 1 "それが型族", \t -> do
	arrowIText t 1 "具体例を挙げないとよくわからないと思う"
 ]

classify :: Page
classify = [\t -> do
	writeTopTitle t "分類"
	text t "", \t -> do
	text t "* 型族(type families)には以下の2つがある", \t -> do
	itext t 1 "- データ族(data families)", \t -> do
	itext t 1 "- 型シノニム族(type synonym families)"
	text t "", \t -> do
	text t "* 上のそれぞれについて関連型(associated)が対応する", \t -> do
	itext t 1 "- 関連データ型(associated data type)", \t -> do
	itext t 1 "- 関連型シノニム(associated type synonym)", \t -> do
	text t "* 関連型は型族の構文糖なので、型族が理解できれば良い"
 ]

dataFamilies :: Page
dataFamilies = [\t -> do
	writeTopTitle t "データ族"
	text t "", \t -> do
	text t "* まずは普通の変数を含むデータ型について見てみる", \t -> do
	text t "* 例としてリストを考えてみよう", \t -> do
	itext t 1 "data [] a = a : ([] a) | []", \t -> do
	itext t 1 "わかりやすく名前を変えてみる", \t -> do
	itext t 1 "data List a = Cons a (List a) | Nil", \t -> do
	text t "* 型aが何であっても同じ構造を共有している", \t -> do
	text t "* 同じ構造でない同じ種類のデータ型も考えられる", \t -> do
	text t "* 違う構造のデータ型を同じ種類の型としてまとめたい"
 ]

identity :: Page
identity = [\t -> do
	writeTopTitle t "データ族"
	text t "", \t -> do
	text t "* たとえば、値のアイデンティティを考えようと思う", \t -> do
	itext t 1 "- 整数のアイデンティティは素因数分解の結果", \t -> do
	itext t 1 "- 文字のアイデンティティは文字の種類と何番目か", \t -> do
	itext t 1 "- と、勝手に決めたとする", \t -> do
	text t "* Identity Intは素因数分解の結果を格納", \t -> do
	text t "* Identity Charは文字の種類と何番目かを格納", \t -> do
	text t "* data Identity a = ...のような定義は不可能", \t -> do
	text t "* 型族を使う"
 ]

identityFamily :: Page
identityFamily = [\t -> do
	writeTopTitle t "データ族"
	text t "", \t -> do
	text t "data family Identity a"
	text t "", \t -> do
	text t "* データ族を宣言した", \t -> do
	itext t 1 "- Identity IntやIdentity Charを別々に定義できる"
	text t "", \t -> do
	text t "data instance Identity Int = PrimeFactors [Int]"
	text t "", \t -> do
	text t "data CharClass = Upper | Lower | Digit"
	text t "data instance Identity Char = CharID CharClass Int"
 ]
