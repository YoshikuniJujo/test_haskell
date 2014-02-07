import Control.Monad
import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第7回 リストを扱う再帰関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	aboutList, aboutList2, aboutList3, aboutList4,
	patternMatch, patternMatch2, patternMatch3, patternMatch4,
	patternMatch5,
	partialFunction, partialFunction2, partialFunction3,
	partialFunction4,
	listSummary,
	listFunctions, listFunctions2, listFunctions3, listFunctions4,
	listFunctions5, listFunctions6, listFunctions7, listFunctions8,
	listFunctions9, listFunctions10, listFunctions11, listFunctions12,
	listFunctions13, listFunctions14, listFunctions15, listFunctions16,
	listFunctions17, listFunctions18, listFunctions19, listFunctions20,
	listFunctions21, listFunctions22,
	listFunctionsSummary,
	otherFunctions,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* リストについて学び、再帰関数について学んだ", \t -> do
	text t "* 「くりかえし」を表現する場合、リストを使うと良い", \t -> do
	text t "* リストのところではリストを扱う関数を使った", \t -> do
	text t "* これらの関数も再帰的に定義されている", \t -> do
	text t "* リストというデータ構造自体、再帰的である", \t -> do
	text t "* リストを扱う関数の再帰的な定義を見ていこう" 
 ]

aboutList :: Page
aboutList = [\t -> do
	writeTopTitle t "そもそもリストとは"
	text t "", \t -> do
	text t "* リストは再帰的なデータ構造である", \t -> do
	text t "* 以下の疑似コードを見てみよう", \t -> do
	itext t 1 "[a] = (a : [a]) or []", \t -> do
	text t "* aのリストとは", \t -> do
	itext t 1 "- aのリストの先頭にa型の値を足したもの", \t -> do
	itext t 1 "- または空リストである", \t -> do
	text t "* これは以下のようにも言える", \t -> do
	itext t 1 "- 空リストはaのリストである", \t -> do
	itext t 1 "- aのリストの先頭にaを追加したものもaのリスト"
 ]

aboutList2 :: Page
aboutList2 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* コマンドプロンプトを2つ立ち上げて", \t -> do
	text t "* lectures/lecture07ディレクトリを作成、そこに移動", \t -> do
	itext t 1 "% ghci", \t -> do
	itext t 1 "Prelude> [] :: [Int]", \t -> do
	itext t 1 $ show ([] :: [Int]), \t -> do
	itext t 1 "Prelude> 3 : [] :: [Int]", \t -> do
	itext t 1 $ show $ (3 : [] :: [Int]), \t -> do
	itext t 1 "Prelude> 8 : 3 : [] :: [Int]", \t -> do
	itext t 1 $ show $ (8 : 3 : [] :: [Int]), \t -> do
	text t "* []から始めて先頭に次々に値を加えていける", \t -> do
	text t "* これがリストの内部構造である"
 ]

aboutList3 :: Page
aboutList3 = [\t -> do
	writeTopTitle t "構文糖"
	text t "", \t -> do
	text t "* 実のところ[1, 2, 3]という表現は構文糖である", \t -> do
	text t "* [1, 2, 3]は1 : 2 : 3 : []と解釈される", \t -> do
	text t "* (:)は今のところは演算子と考えて良い", \t -> do
	text t "* (:)は右結合なので", \t -> do
	itext t 1 "1 : 2 : 3 : []", \t -> do
	arrowIText t 1 "1 : (2 : (3 : []))", \t -> do
	text t "* 慣れてくると[x, y, z]とx : (y : (z : []))とが", \t -> do
	itext t 1 "同じに見えてくるようになる"
 ]

aboutList4 :: Page
aboutList4 = [\t -> do
	writeTopTitle t "構文糖"
	text t "", \t -> do
	text t "* 前に説明したように文字列は文字のリストである", \t -> do
	itext t 1 "\"hello\"", \t -> do
	arrowIText t 1 "['h', 'e', 'l', 'l', 'o']", \t -> do
	text t "* さらにリストの表記に関しても脱糖すると", \t -> do
	arrowIText t 1 "'h' : 'e' : 'l' : 'l' : 'o' : []", \t -> do
	text t "* 括弧を明示すると", \t -> do
	arrowIText t 1 "'h' : ('e' : ('l' : ('l' : ('o' : []))))", \t -> do
	text t "* 慣れてくるとこれらも同じものに見えてくる"
 ]

patternMatch :: Page
patternMatch = [\t -> do
	writeTopTitle t "パターンマッチ"
	text t "", \t -> do
	text t "* リストの内部構造がわかったので", \t -> do
	itext t 1 "リストのパターンマッチも理解できるだろう", \t -> do
	text t "* リストのパターンマッチは以下のようになる", \t -> do
	itext t 1 "fun (x : xs) = ...", \t -> do
	text t "* たとえば[1, 2, 3]を(x : xs)にパターンマッチさせると", \t -> do
	itext t 1 "[1, 2, 3] == 1 : (2 : 3 : [])なので", \t -> do
	itext t 1 "x == 1", \t -> do
	itext t 1 "xs == 2 : 3 : [] == [2, 3]", \t -> do
	text t "* つまりパターンマッチによって", \t -> do
	itext t 1 "リストの先頭とそれ以外に分解できる"
 ]

patternMatch2 :: Page
patternMatch2 = [\t -> do
	writeTopTitle t "パターンマッチ"
	text t "", \t -> do
	text t "* リストには(foo : bar)という形のほかに", \t -> do
	itext t 1 "空リストを表す[]という形がある", \t -> do
	text t "* []は(x : xs)にマッチしないので", \t -> do
	itext t 1 "- fun (x : xs) = ...として", \t -> do
	itext t 1 "- funに空リストを与えるとエラーとなる", \t -> do
	text t "* funを空リストの場合にも使えるようにするには", \t -> do
	itext t 1 "fun [] = ...", \t -> do
	itext t 1 "fun (x : xs) = ..."
 ]

pm3lst1 :: [Int]
pm3lst1 = unsafePerformIO $ do
	sg <- newStdGen
	return $ take 5 $ randomRs (0, 10) sg

pm3str1 :: String
pm3str1 = unsafePerformIO $ do
	sg <- newStdGen
	return $ take 5 $ randomRs ('a', 'z') sg

patternMatch3 :: Page
patternMatch3 = [\t -> do
	writeTopTitle t "headとtail"
	text t "", \t -> do
	text t "* headとtailという関数が用意されている", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 $ "Prelude> head " ++ show pm3lst1, \t -> do
	itext t 1 $ show $ head pm3lst1, \t -> do
	itext t 1 $ "Prelude> tail " ++ show pm3lst1, \t -> do
	itext t 1 $ show $ tail pm3lst1, \t -> do
	itext t 1 $ "Prelude> head " ++ show pm3str1, \t -> do
	itext t 1 $ show $ head pm3str1, \t -> do
	itext t 1 $ "Prelude> tail " ++ show pm3str1, \t -> do
	itext t 1 $ show $ tail pm3str1
 ]

patternMatch4 :: Page
patternMatch4 = [\t -> do
	writeTopTitle t "headとtail"
	text t "", \t -> do
	text t "* headはリストの一番目の要素を返す関数", \t -> do
	text t "* tailはリストの残りの要素を返す関数", \t -> do
	text t "* myList.hsを作り以下の関数を作成しよう", \t -> do
	text t "* 演習7-1. headと同じmyHeadを定義せよ", \t -> do
	text t "* 演習7-2. tailと同じmyTailを定義せよ", \t -> do
	itext t 1 "ヒント: パターンマッチを使おう", \t -> do
	itext t 1 "(1分)"
 ]

patternMatch5 :: Page
patternMatch5 = [\t -> do
	writeTopTitle t "headとtail"
	text t "", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* myHeadは以下のようになる", \t -> do
	itext t 1 "myHead :: [a] -> a", \t -> do
	itext t 1 "myHead (h : _) = h", \t -> do
	text t "* myTailは以下のようになる", \t -> do
	itext t 1 "myTail :: [a] -> [a]", \t -> do
	itext t 1 "myTail (_ : t) = t"
 ]

partialFunction :: Page
partialFunction = [\t -> do
	writeTopTitle t "部分関数"
	text t "", \t -> do
	text t "* myHead, myTailの定義のなかで", \t -> do
	itext t 1 "[]に対する処理が書かれていない", \t -> do
	text t "* よってmyHead, myTailに空リストを与えるとエラーになる", \t -> do
	text t "* エラーとなる値が存在する関数を部分関数と呼ぶ", \t -> do
	text t "* これは標準ライブラリのhead, tailについても同じこと", \t -> do
	text t "* 部分関数は大規模な開発では避けたほうが良い", \t -> do
	text t "* headやtailを使うのは以下のような場合のみにしよう", \t -> do
	itext t 1 "- 引数が絶対に空リストにならない、または", \t -> do
	itext t 1 "- 規模が大きくならない(使い捨ての関数等)"
 ]

myHead :: [a] -> a
myHead (h : _) = h
myHead _ = error "bad"

myTail :: [a] -> [a]
myTail (_ : t) = t
myTail _ = error "bad"

partialFunction2 :: Page
partialFunction2 = [\t -> do
	writeTopTitle t "試してみよう"
	text t "", \t -> do
	itext t 1 "% ghci myList.hs", \t -> do
	itext t 1 $ "*Main> myHead " ++ show pm3lst1, \t -> do
	itext t 1 $ show $ myHead pm3lst1, \t -> do
	itext t 1 $ "*Main> myTail " ++ show pm3lst1, \t -> do
	itext t 1 $ show $ myTail pm3lst1, \t -> do
	itext t 1 $ "*Main> myHead " ++ show pm3str1, \t -> do
	itext t 1 $ show $ myHead pm3str1, \t -> do
	itext t 1 $ "*Main> myTail " ++ show pm3str1, \t -> do
	itext t 1 $ show $ myTail pm3str1
 ]

partialFunction3 :: Page
partialFunction3 = [\t -> do
	writeTopTitle t "試してみよう"
	text t "", \t -> do
	text t "*Main> myHead []", \t -> do
	text t "*** Exception: .../myList.hs:X:Y-Z:"
	itext t 1 "Non-exhaustive patterns in function myHead", \t -> do
	text t "*Main> myTail []", \t -> do
	text t "*** Exception: .../myList.hs:X:Y-Z:"
	itext t 1 "Non-exhaustive patterns in function myTail", \t -> do
	text t "* エラーメッセージをわかりやすくするには", \t -> do
	itext t 1 "myHead (h : _) = h", \t -> do
	itext t 1 "myHead [] = error \"myHead: empty list\"", \t -> do
	itext t 1 "myTail (_ : t) = t", \t -> do
	itext t 1 "myTail [] = error \"myTail: empty list\""
 ]

partialFunction4 :: Page
partialFunction4 = [\t -> do
	writeTopTitle t "試してみよう"
	text t "", \t -> do
	text t "*Main> :reload", \t -> do
	text t "*Main> myHead []", \t -> do
	text t "*** Exception: myHead: empty list", \t -> do
	text t "*Main> myTail []", \t -> do
	text t "*** Exception: myTail: empty list"
 ]

listSummary :: Page
listSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* リストの構造について説明した", \t -> do
	itext t 1 "- リストとは[](空リスト)、または", \t -> do
	itext t 1 "- リストの先頭に値を足したもの", \t -> do
	text t "* [1, 2, 3]という表記は構文糖で", \t -> do
	itext t 1 "1 : (2 : (3 : []))ということ", \t -> do
	text t "* []および(x : xs)でパターンマッチができる", \t -> do
	text t "* headとtailの例を見た", \t -> do
	itext t 1 "- headとtailは部分関数", \t -> do
	itext t 1 "- 部分関数は大規模な開発では使わないほうが良い"
 ]

listFunctions :: Page
listFunctions = [\t -> do
	writeTopTitle t "総和を求める関数"
	text t "", \t -> do
	text t "* リストの再帰関数について学んでいこう", \t -> do
	text t "* まずはリストの要素の総和を求める関数sumを見よう", \t -> do
	text t "* こう考える", \t -> do
	itext t 1 "- 空リストの総和は0", \t -> do
	itext t 1 "- 値xをリストxsに追加したリストの総和は", \t -> do
	itext t 2 "x + リストxsの総和", \t -> do
	text t "* そのまま書けば良い", \t -> do
	text t "* myList.hsに以下を書き込もう", \t -> do
	itext t 1 "mySum :: [Int] -> Int", \t -> do
	itext t 1 "mySum [] = 0", \t -> do
	itext t 1 "mySum (x : xs) = x + mySum xs"
 ]

mySum :: [Int] -> Int
mySum [] = 0
mySum (x : xs) = x + mySum xs

lf2lst1, lf2lst2 :: [Int]
[lf2lst1, lf2lst2] = unsafePerformIO $ replicateM 2 $ do
	sg <- newStdGen
	return $ take 5 $ randomRs (0, 10) sg

listFunctions2 :: Page
listFunctions2 = [\t -> do
	writeTopTitle t "試してみよう"
	text t "", \t -> do
	text t "*Main> :reload", \t -> do
	text t $ "*Main> mySum " ++ show lf2lst1, \t -> do
	text t $ show $ mySum lf2lst1, \t -> do
	text t $ "*Main> mySum " ++ show lf2lst2, \t -> do
	text t $ show $ mySum lf2lst2
 ]

listFunctions3 :: Page
listFunctions3 = [\t -> do
	writeTopTitle t "総積を求める関数"
	text t "", \t -> do
	text t "* リストの要素すべてをかけ合わせた値を返す関数がある", \t -> do
	itext t 1 "- これを総積と呼ぼう", \t -> do
	text t "* productという名前で用意されているが自分で定義しよう", \t -> do
	text t "* 演習7-3. リストの総積を求める関数myProductを定義せよ", \t -> do
	itext t 1 "(1分)"
 ]

listFunctions4 :: Page
listFunctions4 = [\t -> do
	writeTopTitle t "総積を求める関数"
	text t "", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* 順を追って見ていこう", \t -> do
	itext t 1 "- 空リストの総積は1", \t -> do
	itext t 1 "- 値xをリストxsに追加したリストの総積は", \t -> do
	itext t 2 "値x * リストxsの総積", \t -> do
	text t "* これをそのまま書けば良い", \t -> do
	itext t 1 "myProduct :: [Int] -> Int", \t -> do
	itext t 1 "myProduct [] = 1", \t -> do
	itext t 1 "myProduct (x : xs) = x * myProduct xs", \t -> do
	text t "* myList.hsに書き込んでおこう"
 ]

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

lf5lst1, lf5lst2 :: [Int]
[lf5lst1, lf5lst2] = unsafePerformIO $ replicateM 2 $ do
	sg <- newStdGen
	return $ take 5 $ randomRs (1, 7) sg

listFunctions5 :: Page
listFunctions5 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t $ "*Main> :reload", \t -> do
	text t $ "*Main> myProduct " ++ show lf5lst1, \t -> do
	text t $ show $ myProduct lf5lst1, \t -> do
	text t $ "*Main> myProduct " ++ show lf5lst2, \t -> do
	text t $ show $ myProduct lf5lst2
 ]

listFunctions6 :: Page
listFunctions6 = [\t -> do
	writeTopTitle t "リストの長さを求める関数"
	text t "", \t -> do
	text t "* 演習7-4. リストの長さを求めるmyLengthを定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* これも同じように考えれば良い", \t -> do
	itext t 1 "- 空リストの長さは0", \t -> do
	itext t 1 "- 何かの値をリストxsに追加したリストの長さは", \t -> do
	itext t 2 "1 + リストxsの長さ", \t -> do
	text t "* 値自体はいじらないので型は何でもいい", \t -> do
	itext t 1 "myLength :: [a] -> Int", \t -> do
	itext t 1 "myLength [] = 0", \t -> do
	itext t 1 "myLength (_ : xs) = 1 + myLength xs"
 ]

listFunctions7 :: Page
listFunctions7 = [\t -> do
	writeTopTitle t "[]と(:)を置き換える"
	text t "", \t -> do
	text t "* mySumの定義をもう一度見てみよう", \t -> do
	itext t 1 "mySum [] = 0", \t -> do
	itext t 1 "mySum (x : xs) = x + mySum xs", \t -> do
	text t "* 見かたを変えるとこう読むこともできる", \t -> do
	itext t 1 "- mySumは[]を0に置き換え", \t -> do
	itext t 1 "- (:)を(+)に置き換える関数", \t -> do
	text t "* mySum [1, 2, 3, 4]は以下の変換を行っている", \t -> do
	itext t 1 "1 : (2 : (3 : (4 : [])))", \t -> do
	arrowIText t 1 "1 + (2 + (3 + (4 + 0)))"
 ]

listFunctions8 :: Page
listFunctions8 = [\t -> do
	writeTopTitle t "[]と(:)を置き換える"
	text t "", \t -> do
	text t "* これは一般化することができる", \t -> do
	itext t 1 "fun [] = v", \t -> do
	itext t 1 "fun (x : xs) = x `op` xs", \t -> do
	text t "* funは[]をvに(:)をopに置き換える関数である", \t -> do
	itext t 1 "x0 : (x1 : (x2 : (... (xn : [])..)))", \t -> do
	arrowIText t 1 "x0 `op` (x1 `op` (x2 `op` (... (xn `op` v)..)))"
 ]

listFunctions9 :: Page
listFunctions9 = [\t -> do
	writeTopTitle t "右結合と左結合"
	text t "", \t -> do
	text t "* 加算や乗算では右結合と左結合で値は変わらない", \t -> do
	itext t 1 "1 + (2 + 3) == (1 + 2) + 3", \t -> do
	text t "* しかし減算や除算では右結合と左結合で値が変わる", \t -> do
	itext t 1 "1 - (2 - 3) /= (1 - 2) - 3", \t -> do
	text t "* 一般的に四則演算では演算子は左結合する", \t -> do
	text t "* まずは左結合する総和関数の作りかたを見てみよう"
 ]

listFunctions10 :: Page
listFunctions10 = [\t -> do
	writeTopTitle t "右結合と左結合"
	text t "", \t -> do
	text t "* mySumは以下の変換を行う", \t -> do
	itext t 1 "x0 : x1 : x2 : ... : []", \t -> do
	arrowIText t 1 "x0 + (x1 + (x2 + (... + 0)))", \t -> do
	text t "* 以下のような変換を行うmySum'を作りたい", \t -> do
	itext t 1 "x0 : x1 : x2 : ... : []", \t -> do
	arrowIText t 1 "((0 + x0) + x1) + x2) ..."
 ]

listFunctions11 :: Page
listFunctions11 = [\t -> do
	writeTopTitle t "蓄積変数"
	text t "", \t -> do
	text t "* 結果の形を再掲する", \t -> do
	itext t 1 "((0 + x0) + x1) + x2) ...", \t -> do
	text t "* 以下のことがわかる", \t -> do
	itext t 1 "- x1の計算をするとき(0 + x0)が必要", \t -> do
	itext t 1 "- x2の計算をするとき((0 + x0) + x1)が必要", \t -> do
	itext t 2 "...", \t -> do
	text t "* これらの値を保存しておく変数が必要になる", \t -> do
	itext t 1 "- これを蓄積変数と呼ぶ"
 ]

listFunctions12 :: Page
listFunctions12 = [\t -> do
	writeTopTitle t "左結合の総和"
	text t "", \t -> do
	text t "* mySum'の定義は以下のようになる", \t -> do
	itext t 1 "mySum' :: [Int] -> Int", \t -> do
	itext t 1 "mySum' = mySumIter 0"
	itext t 1 "", \t -> do
	itext t 1 "mySumIter :: Int -> [Int] -> Int", \t -> do
	itext t 1 "mySumIter s [] = s", \t -> do
	itext t 1 "mySumIter s (x : xs) = mySumIter (s + x) xs", \t -> do
	text t "* sが蓄積変数", \t -> do
	text t "* mySumIterはsにリストの要素の値を足していき", \t -> do
	itext t 1 "リストが空になったらsを返す"
 ]

listFunctions13 :: Page
listFunctions13 = [\t -> do
	writeTopTitle t "反復的処理"
	text t "", \t -> do
	text t "* mySumIterの定義を見てみる", \t -> do
	itext t 1 "mySumIter s (x : xs) = mySumIter (s + x) xs", \t -> do
	text t "* 引数だけ変化した同じ関数に完全に置き換えられている", \t -> do
	itext t 1 "mySumIter 0 [1, 2, 3]", \t -> do
	arrowIText t 1 "mySumIter 1 [2, 3]", \t -> do
	arrowIText t 1 "mySumIter 3 [3]", \t -> do
	arrowIText t 1 "mySumIter 6 []", \t -> do
	arrowIText t 1 "6", \t -> do
	text t "* これは内部的には単純な「くりかえし」として実行できる", \t -> do
	text t "* この関数は再帰的に定義されてはいるが", \t -> do
	itext t 1 "- 「反復的処理」を行っていることになる"
 ]

listFunctions14 :: Page
listFunctions14 = [\t -> do
	writeTopTitle t "再帰的処理と反復的処理"
	text t "", \t -> do
	text t "* mySumの定義を見てみる", \t -> do
	itext t 1 "mySum (x : xs) = x + mySum xs", \t -> do
	text t "* この定義は引数だけ変えた置き換えではない", \t -> do
	arrowIText t 1 "単純な「くりかえし」には変換できない", \t -> do
	text t "* このような処理を「再帰的処理」と呼ぶ", \t -> do
	text t "* ここまで見てきたように", \t -> do
	itext t 1 "- リストに対する再帰的処理は右結合の結果を返し", \t -> do
	itext t 1 "- リストに対する反復的処理は左結合の結果を返す"
 ]

listFunctions15 :: Page
listFunctions15 = [\t -> do
	writeTopTitle t "再帰的処理を抽象化"
	text t "", \t -> do
	text t "* リストに対する再帰的処理の多くは以下の形をとる", \t -> do
	itext t 1 "fun [] = v", \t -> do
	itext t 1 "fun (x : xs) = x `op` fun xs", \t -> do
	text t "* この枠組みでvとopだけ変えればいろいろな関数が作れる", \t -> do
	text t "* そのような枠組みはHaskellでは関数として抽象化できる", \t -> do
	itext t 1 "foldr op v [] = v", \t -> do
	itext t 1 "foldr op v (x : xs) = x `op` foldr op v xs"
 ]

listFunctions16 :: Page
listFunctions16 = [\t -> do
	writeTopTitle t "再帰的処理を抽象化"
	text t "", \t -> do
	text t "* 理解のためにopに(+)をvに0をいれてみよう", \t -> do
	itext t 1 "foldr (+) 0 [] = 0", \t -> do
	itext t 1 "foldr (+) 0 (x : xs) = x + foldr (+) 0 xs", \t -> do
	text t "* これとmySumの定義を見くらべると", \t -> do
	itext t 1 "mySum [] = 0", \t -> do
	itext t 1 "mySum (x : xs) = x + mySum xs", \t -> do
	text t "* 以下の関係が成り立つことがわかる", \t -> do
	itext t 1 "mySum == foldr (+) 0"
 ]

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _op v [] = v
myFoldr op v (x : xs) = x `op` myFoldr op v xs

listFunctions17 :: Page
listFunctions17 = [\t -> do
	writeTopTitle t "再帰的処理を抽象化"
	text t "", \t -> do
	text t "* foldrは標準ライブラリに用意されているので", \t -> do
	text t "* myFoldrとしてmyList.hsに以下を書き込もう", \t -> do
	itext t 1 "myFoldr op v [] = v", \t -> do
	itext t 1 "myFoldr op v (x : xs) = x `op` myFoldr op v xs", \t -> do
	text t "* 使ってみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> myFoldr (+) 0 [1, 2, 3, 4]", \t -> do
	itext t 1 $ show $ myFoldr (+) 0 [1 :: Int, 2, 3, 4], \t -> do
	itext t 1 "*Main> myFoldr (*) 1 [1, 2, 3, 4]", \t -> do
	itext t 1 $ show $ myFoldr (*) 1 [1 :: Int, 2, 3, 4]
 ]

listFunctions18 :: Page
listFunctions18 = [\t -> do
	writeTopTitle t "foldrの型"
	text t "", \t -> do
	text t "* foldrの型について考えてみよう", \t -> do
	itext t 1 "foldr op v [] = v", \t -> do
	itext t 1 "foldr op v (x : xs) = x `op` foldr op v xs", \t -> do
	text t "* foldr :: X -> Y -> Z -> Rとする", \t -> do
--	text t "* 第1引数, 第2引数, 第3引数, 返り値の型をそれぞれ", \t -> do
--	itext t 1 "X, Y, Z, Rとする", \t -> do
	text t "* 第3引数は「何かのリスト」なのでZ = [a]と置ける", \t -> do
	text t "* 第2引数vがそのまま返り値となるのでY = Rでこれをbと置く", \t -> do
	text t "* x `op` foldr op v xsが全体の返り値となるのでopは", \t -> do
	itext t 1 "- (x :: a)と(foldr op v :: R)を引数としてとり", \t -> do
	itext t 1 "- Rを返り値として返すので", \t -> do
	itext t 1 "X = (a -> b -> b)", \t -> do
	arrowIText t 0 "foldr :: (a -> b -> b) -> b -> [a] -> b"
 ]

listFunctions19 :: Page
listFunctions19 = [\t -> do
	writeTopTitle t "反復的処理の抽象化"
	text t "", \t -> do
	text t "* 反復的処理の枠組み", \t -> do
	itext t 1 "fun s [] = s", \t -> do
	itext t 1 "fun s (x : xs) = fun (s `op` x) xs", \t -> do
	text t "* この枠組みを関数にする", \t -> do
	itext t 1 "foldl op s [] = s", \t -> do
	itext t 1 "foldl op s (x : xs) = foldl op (s `op` x) xs"
 ]

listFunctions20 :: Page
listFunctions20 = [\t -> do
	writeTopTitle t "反復的処理の抽象化"
	text t "", \t -> do
	text t "* 逐次的に評価してみる", \t -> do
	itext t 1 "foldl op v [x, y, z]", \t -> do
	arrowIText t 1 "foldl op v (x : y : z : [])", \t -> do
	arrowIText t 1 "foldl op (v `op` x) (y : z : [])", \t -> do
	arrowIText t 1 "foldl op ((v `op` x) `op` y) (z : [])", \t -> do
	arrowIText t 1 "foldl op (((v `op` x) `op` y) `op` z) []", \t -> do
	arrowIText t 1 "((v `op` x) `op` y) `op` z"
 ]

listFunctions21 :: Page
listFunctions21 = [\t -> do
	writeTopTitle t "foldlの型"
	text t "", \t -> do
	text t "foldl op s [] = s"
	text t "foldl op s (x : xs) = foldl op (s `op` x) xs", \t -> do
	text t "* foldl :: X -> Y -> Z -> Rとする", \t -> do
	text t "* 第三引数は何らかのリストなのでZ == [b]とできる", \t -> do
	text t "* 第二引数はそのまま返り値になるのでY == Rでaと置く", \t -> do
	text t "* (s `op` x)が第二引数になるので", \t -> do
	itext t 1 "- opの第一引数はsなので型Yつまりa", \t -> do
	itext t 1 "- opの第二引数はxなので型b", \t -> do
	itext t 1 "- opの返り値はfoldlの第二引数なので型Yつまりa", \t -> do
	itext t 1 "- よって op :: a -> b -> a", \t -> do
	arrowIText t 0 "foldl :: (a -> b -> a) -> a -> [b] -> a"
 ]

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ s [] = s
myFoldl op s (x : xs) = myFoldl op (s `op` x) xs

listFunctions22 :: Page
listFunctions22 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* myList.hsに以下を書き込もう", \t -> do
	itext t 1 "myFoldl :: (a -> b -> a) -> a -> [b] -> a", \t -> do
	itext t 1 "myFoldl op s [] = s", \t -> do
	itext t 1 "myFoldl op s (x : xs) = myFoldl op (s `op` x) xs", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> myFoldl (-) 10 [1, 2, 3]", \t -> do
	itext t 1 $ show $ myFoldl (-) 10 [1 :: Int, 2, 3], \t -> do
	text t "* myFoldl (-) 10 [1, 2, 3]は以下のようになる", \t -> do
	itext t 1 "((10 - 1) - 2) - 3", \t -> do
	text t "* 左結合なので()は省略できて", \t -> do
	itext t 1 "10 - 1 - 2 - 3"
 ]

listFunctionsSummary :: Page
listFunctionsSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* sum, product, lengthなどは再帰的な定義が可能", \t -> do
	text t "* リストを扱う再帰的関数の多くは", \t -> do
	itext t 1 "- 要素間に同一の演算子を挿入した形となる", \t -> do
	text t "* 演算子の適用のしかたには右結合と左結合とがある", \t -> do
	text t "* そのような構造は関数によって抽象化できる", \t -> do
	itext t 1 "- foldr: 右結合演算子の挿入", \t -> do
	itext t 1 "- foldl: 左結合演算子の挿入"
 ]

listFunctionsX :: Page
listFunctionsX = [\t -> do
	writeTopTitle t "右結合と左結合"
	text t "", \t -> do
	text t "* sum [1, 2, 3, 4, 5]は以下のようになる", \t -> do
	itext t 1 "1 + (2 + (3 + (4 + (5 + 0))))", \t -> do
	text t "* つまり右結合である", \t -> do
	text t "* 通常、四則演算の演算子は左結合である", \t -> do
	text t "* 足し算なら値は変わらないが", \t -> do
	text t "* 引き算では以下の2つの値は異なる", \t -> do
	itext t 1 "1 - (2 - 3)", \t -> do
	itext t 1 "(1 - 2) - 3"
 ]

otherFunctions :: Page
otherFunctions = [\t -> do
	writeTopTitle t "その他の関数"
	text t "", \t -> do
	text t "* Haskellにはリストを扱う関数が豊富に用意されている", \t -> do
	text t "* 再帰的な定義によってどの関数も作ることができる", \t -> do
	text t "* 以下の関数については演習で扱う予定", \t -> do
	itext t 1 "take, drop, concat, reverse, map, filter", \t -> do
	text t "* それぞれの関数の再帰的定義を見る", \t -> do
	text t "* 可能なものについてはfoldによる定義も見る"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* リストの内部構造を学んだ", \t -> do
	text t "* リストに対するパターンマッチを見た", \t -> do
	text t "* リストを扱う再帰関数の定義のしかたを学んだ", \t -> do
	text t "* リストを扱う関数の多くがあてはまる枠組を見た", \t -> do
	itext t 1 "- 要素間に演算子を入れるという枠組", \t -> do
	itext t 1 "- 右結合と左結合とがある", \t -> do
	text t "* その枠組自体が関数で表せることを学んだ", \t -> do
	itext t 1 "- foldr: 右結合", \t -> do
	itext t 1 "- foldl: 左結合"
 ]
