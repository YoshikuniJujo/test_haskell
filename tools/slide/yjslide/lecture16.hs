module Main where

import Lecture

subtitle :: String
subtitle = "第16回 型族"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, whats, classify,
	dataFamilies, identity, identityFamily,
	identityFunInt, identityFunInt2, identityFunChar, identityFuns,
	identityClass, identityClassFun, dataFamilySummary,
	associatedDataType, associatedDataType2, associatedDataTypeSummary,
	typeSynonymFamilies
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
	text t "* ソースコードに{-# LANGUAGE TypeFamilies #-}をつける", \t -> do
	text t "* 型族にはいろいろな用途がある", \t -> do
	itext t 1 "- 今回は型クラスでの使用という側面にしぼって紹介"
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
	text t "* まずは普通の「変数を含むデータ型」について見てみる", \t -> do
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
	itext t 1 "deriving Show"
	text t "", \t -> do
	text t "data CharClass = Upper | Lower | Digit deriving Show"
	text t "data instance Identity Char = CharID CharClass Int"
	itext t 1 "deriving Show"
 ]

identityFunInt :: Page
identityFunInt = [\t -> do
	writeTopTitle t "データ族"
	text t "", \t -> do
	text t "primeFactors :: Int -> Int -> [Int]"
	text t "primeFactors f n"
	itext t 1 "| f <= 0 || n <= 0 = []"
	itext t 1 "| f > n = []"
	itext t 1 "| n `mod` f == 0 = f : primeFactors f (n `div` f)"
	itext t 1 "| otherwise = primeFactors (f + 1) n"
	text t "", \t -> do
	text t "toIdentityInt :: Int -> Maybe (Identity Int)"
	text t "toIdentityInt n"
	itext t 1 "| n > 0 = Just $ PrimeFactors $ primeFactors 2 n"
	itext t 1 "| otherwise = Nothing"
 ]

identityFunInt2 :: Page
identityFunInt2 = [\t -> do
	writeTopTitle t "データ族"
	text t "", \t -> do
	text t "fromIdentityInt :: Identity Int -> Int"
	text t "fromIdentityInt (PrimeFactors pfs) = product pfs"
	text t "", \t -> do
	text t "* IntとIdentity Intを相互変換する関数を定義した"
 ]

identityFunChar :: Page
identityFunChar = [\t -> do
	writeTopTitle t "データ族"
	text t "", \t -> do
	text t "toIdentityChar :: Char -> Maybe (Identity Char)"
	text t "toIdentityChar c"
	itext t 0.5 "| isUpper c = Just $ CharID Upper $ ord c - ord 'A'"
	itext t 0.5 "| isLower c = Just $ CharID Lower $ ord c - ord 'a'"
	itext t 0.5 "| isDigit c = Just $ CharID Digit $ ord c - ord '0'"
	itext t 0.5 "| otherwise = Nothing"
	text t "", \t -> do
	text t "fromIdentityChar :: Identity Char -> Char"
	text t "fromIdentityChar (CharID Upper n) = chr $ ord 'A' + n"
	text t "fromIdentityChar (CharID Lower n) = chr $ ord 'a' + n"
	text t "fromIdentityChar (CharID Digit n) = chr $ ord '0' + n"
 ]

identityFuns :: Page
identityFuns = [\t -> do
	writeTopTitle t "データ族"
	text t "", \t -> do
	text t "* IntとIdentity Intとを相互に変換する関数を定義した", \t -> do
	itext t 1 "toIdentityInt :: Int -> Myabe (Identity Int)"
	itext t 1 "fromIdentityInt :: Identity Int -> Int", \t -> do
	text t "* CharとIdentity Charとを相互に変換する関数を定義した", \t -> do
	itext t 1 "toIdentityChar :: Char -> Maybe (Identity Char)"
	itext t 1 "fromIdentityChar :: Identity Char -> Char", \t -> do
	text t "* 実のところここまでなら型族を使うメリットは少ない", \t -> do
	itext t 1 "- IdentityIntとIdentityCharを使っても同じ", \t -> do
	itext t 1 "- コードがわかりやすくなるというメリットはある", \t -> do
	text t "* 型クラスと一緒に使うと本領を発揮する"
 ]

identityClass :: Page
identityClass = [\t -> do
	writeTopTitle t "データ族"
	text t "", \t -> do
	text t "class HaveIdentity a where"
	itext t 1 "toIdentity :: a -> Maybe (Identity a)"
	itext t 1 "fromIdentity :: Identity a -> a"
	text t "", \t -> do
	text t "instance HaveIdentity Int where"
	itext t 1 "toIdentity = toIdentityInt"
	itext t 1 "fromIdentity = fromIdentityInt"
	text t "", \t -> do
	text t "instance HaveIdentity Char where"
	itext t 1 "toIdentity = toIdentityChar"
	itext t 1 "fromIdentity = fromIdentityChar"
 ]

identityClassFun :: Page
identityClassFun = [\t -> do
	writeTopTitle t "データ族"
	text t "", \t -> do
	text t "* HaveIdentityを使った関数の例", \t -> do
	itext t 1 "- FlexibleContexts拡張が必要", \t -> do
	text t "", \t -> do
	text t "printIdentity ::"
	itext t 1 "(HaveIdentity a, Show (Identity a)) => a -> IO ()"
	text t "printIdentity x = case toIdentity x of"
	itext t 1 "Just i -> print i"
	itext t 1 "Nothing -> putStrLn \"no identity\""
 ]

dataFamilySummary :: Page
dataFamilySummary = [\t -> do
	writeTopTitle t "データ族(まとめ)"
	text t "", \t -> do
	text t "* 型aのリストは[] aである", \t -> do
	itext t 1 "- [] aはどんな型aに対しても同じ構造", \t -> do
	text t "* 型aのアイデンティティはIdentity aである", \t -> do
	itext t 1 "- Identity aは型aによって違う構造を取る", \t -> do
	text t "* Identityが型族であることを宣言する", \t -> do
	itext t 1 "data family Identity a", \t -> do
	text t "* 型族のインスタンスを定義する", \t -> do
	itext t 1 "data instance Identity Int = ..."
	itext t 1 "data instance Identity Char = ...", \t -> do
	text t "* 型族は型クラスのなかで使ったときに本領を発揮する"
 ]

associatedDataType :: Page
associatedDataType = [\t -> do
	writeTopTitle t "関連データ型"
	text t "", \t -> do
	text t "* 関連データ型(associated data type)はデータ族の構文糖", \t -> do
	text t "* 型族は型クラスのなかで使われることが多い", \t -> do
	text t "* 型クラスと関連して使われるデータ族を特別扱いする構文", \t -> do
	text t "* 関連データ型として宣言されたデータ族は", \t -> do
	itext t 1 "- 省略された構文が使える", \t -> do
	itext t 1 "- それが宣言されたクラス内でのみ使用可", \t -> do
	text t "* さっきの例を関連データ型で定義してみる"
 ]

associatedDataType2 :: Page
associatedDataType2 = [\t -> do
	writeTopTitle t "関連データ型"
	text t "", \t -> do
	text t "class HaveIdentity a where"
	itext t 1 "data Identity a"
	itext t 1 "toIdentity :: a -> Maybe (Identity a)"
	itext t 1 "fromIdentity :: Identity a -> a"
	text t "", \t -> do
	text t "instance HaveIdentity Int where"
	itext t 1 "data Identity Int = PrimeFuctors [Int]"
	itext t 1 "...", \t -> do
	text t "instance HaveIdentity Char where"
	itext t 1 "data Identity Char = CharID CharClass Int"
	itext t 1 "..."
 ]

associatedDataTypeSummary :: Page
associatedDataTypeSummary = [\t -> do
	writeTopTitle t "関連データ型(まとめ)"
	text t "", \t -> do
	text t "* 関連データ型はデータ族の構文糖", \t -> do
	itext t 1 "- 宣言されたクラスでのみ使用可という制限がある", \t -> do
	text t "* データ族の宣言はclass定義のなかで行われる", \t -> do
	itext t 1 "- familyは省略される", \t -> do
	text t "* データ族のインスタンスの定義はinstance定義のなかで", \t -> do
	itext t 1 "- instanceは省略される"
 ]

typeSynonymFamilies :: Page
typeSynonymFamilies = [\t -> do
	writeTopTitle t "型シノニム族"
	text t "", \t -> do
	text t ""
 ]
