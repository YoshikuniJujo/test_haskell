import Lecture

subtitle :: String
subtitle = "第41回 unbox型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	getPi, getPi2, getPi3, getPi4, getPi5,
	usage, usage2, usage3, summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ghcにおいて普通の値はbox化された値である", \t -> do
	text t "* これは遅延評価のためにraw bitの表現を包み込んである", \t -> do
	text t "* そのraw bitの表現がunbox型である", \t -> do
	text t "* unbox化された値を使うと効率が向上することがある", \t -> do
	text t "* ここらへんの最適化は(-O)でghcがしてくれる", \t -> do
	text t "* ghcの最適化がうまく動かないときだけunbox型を使う"
 ]

getPi :: Page
getPi = [\t -> do
	writeTopTitle t "ライプニッツの公式"
	text t "", \t -> do
	text t "* 以下の公式が知られている", \t -> do
	itext t 1 "pi/4 = 1 - 1/3 + 1/5 - 1/7 + 1/9 - ...", \t -> do
	text t "* これを実装してみよう", \t -> do
	itext t 1 "getPi4 :: Double -> Double -> Word -> Double"
	itext t 1 "getPi4 p _ 0 = p"
	itext t 1 "getPi4 p i n = getPi4 (p + recip i)"
	itext t 2 "(negate $ i + signum i * 2) (n - 1)", \t -> do
	text t "* このままだと空間効率が悪いので正格評価させる"
 ]

getPi2 :: Page
getPi2 = [\t -> do
	writeTopTitle t "ライプニッツの公式"
	text t "", \t -> do
	text t "* 正格評価版", \t -> do
	itext t 1 "getPi4 p _ 0 = p"
	itext t 1 "getPi4 p i n = let"
	itext t 2 "p' = p + recip i"
	itext t 2 "i' = negate $ i + signum i * 2 in"
	itext t 2 "getPi4 p' i' (n - 1)", \t -> do
	text t "* 1億回計算させてみよう", \t -> do
	itext t 1 "main :: IO ()"
	itext t 1 "main = print $ 4 * getPi4 0 1 (10 ^ 8)", \t -> do
	text t "* 20.27秒かかる"
 ]

getPi3 :: Page
getPi3 = [\t -> do
	writeTopTitle t "ライプニッツの公式", \t -> do
	text t "* 同じことをunbox型でやってみる", \t -> do
	itext t 1 "getPi4 :: Double# -> Double# -> Word# -> Double#"
	itext t 1 "getPi4 p _ 0## = p"
	itext t 1 "getPi4 p i n = getPi4 (p +## (1.0## /## i))"
	itext t 2 "(negateDouble# (i +##"
	itext t 3 "(signumDouble# i *## 2.0##)))"
	itext t 2 "(n `minusWord#` 1##)", \t -> do
	text t "* signumDouble#はlibraryにないので自分で定義", \t -> do
	itext t 1 "signumDouble# :: Double# -> Double#"
	itext t 1 "signumDouble# x = let"
	itext t 2 "(# s, _, _, _ #) = decodeDouble_2Int# x in"
	itext t 2 " int2Double# s"
 ]

getPi4 :: Page
getPi4 = [\t -> do
	writeTopTitle t "ライプニッツの公式"
	text t "", \t -> do
	text t "* 同様に1億回の計算をさせてみる", \t -> do
	itext t 1 "main :: IO ()", \t -> do
	itext t 1 "main = let !(W# t) = 10 ^ 8 in"
	itext t 2 "print $ 4 * (D# (getPi4 0.0## 1.0## t))", \t -> do
	text t "* 必要な言語拡張", \t -> do
	itext t 1 "MagicHash, UnboxedTuples, BangPatterns", \t -> do
	text t "* 必要なモジュール", \t -> do
	itext t 1 "Data.Word, GHC.Prim, GHC.Types", \t -> do
	text t "* これにかかる時間が3.61秒", \t -> do
	itext t 1 "- 5倍以上の高速化"
 ]

getPi5 :: Page
getPi5 = [\t -> do
	writeTopTitle t "ライプニッツの公式"
	text t "", \t -> do
	text t "* しかし、今回の場合", \t -> do
	itext t 1 "- ひとつめのコードを(-O)でコンパイルすると3.62秒", \t -> do
	itext t 1 "- unbox型を使った場合と同じ速度が出る", \t -> do
	text t "* (-O)による最適化はbox化された値をunbox化してくれる", \t -> do
	arrowIText t 1 "明示的にunbox型を使う必要はない", \t -> do
	text t "* unbox型による最適化は", \t -> do
	itext t 1 "- まずは(-O)を試してみること", \t -> do
	itext t 1 "- (-O)による最適化が利かない特殊な場合だけ使う"
 ]

usage :: Page
usage = [\t -> do
	writeTopTitle t "使いかた"
	text t "", \t -> do
	text t "* MagicHash言語拡張を使う", \t -> do
	itext t 1 "- #を名前に使えるようにするために", \t -> do
	text t "* GHC.Primをimportする", \t -> do
	text t "* リテラルはそれぞれの型別にある", \t -> do
	itext t 1 "'c'# :: Char#", \t -> do
	itext t 1 "3# :: Int#", \t -> do
	itext t 1 "3## :: Word#", \t -> do
	itext t 1 "3.0# :: Float#", \t -> do
	itext t 1 "3.0## :: Double#"
 ]

usage2 :: Page
usage2 = [\t -> do
	writeTopTitle t "使いかた"
	text t "", \t -> do
	text t "* box化するにはGHC.Typesをimportする", \t -> do
	itext t 1 "C# 'c'# :: Char", \t -> do
	itext t 1 "I# 3# :: Int", \t -> do
	itext t 1 "W# 3## :: Word", \t -> do
	itext t 1 "F# 3.0# :: Float", \t -> do
	itext t 1 "D# 3.0## :: Double", \t -> do
	text t "* unbox化するためにはデータ構築子をはいでやる", \t -> do
	itext t 1 "let !(I# i) = 3 in ..."
 ]

usage3 :: Page
usage3 = [\t -> do
	writeTopTitle t "使いかた"
	text t "", \t -> do
	text t "* その他GHC.Primモジュールのそれぞれの演算子を使う", \t -> do
	itext t 1 "(+#) :: Int# -> Int# -> Int#", \t -> do
	itext t 1 "plusWord# :: Word# -> Word# -> Word#", \t -> do
	itext t 1 "plusFloat# :: Float# -> Float# -> Float#", \t -> do
	itext t 1 "(+##) :: Double# -> Double# -> Double#", \t -> do
	text t "* 一般の関数は使えない", \t -> do
	itext t 1 "- 普通の関数における型は*で表現される型である", \t -> do
	itext t 1 "- unbox型は#で表現される型", \t -> do
	text t "* 最終的にはbox化して使用することになる"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* unbox型の値について見てきた", \t -> do
	text t "* 普段は気にする必要がない", \t -> do
	text t "* (-O)による最適化でunbox型を使うのと同じ効果", \t -> do
	text t "* まれに最適化が利かないときがある", \t -> do
	text t "* そのような場合にunbox型を明示的に使うこともある"
 ]
