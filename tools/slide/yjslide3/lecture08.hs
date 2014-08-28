import Control.Applicative
import Data.List
import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第8回 リストを作成する再帰関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	enumerate, aboutEnumFromTo, aboutEnumFromTo2, aboutEnumFromTo3,
	aboutEnumFromToSummary,
	collatz1, collatz2, collatz3, collatz4, collatz5, collatz6, collatz7,
	collatz8, collatz9, collatz10, collatz11, collatz12, collatz13,
	collatz14, collatz15, collatz16, collatz17, collatz18, collatz19,
	collatz20, collatzSummary,
	factorization1, factorization2, factorization3, factorization4,
	factorization5, factorization6, factorization7, factorization8,
	factorization9, factorization10, factorization11, factorization12,
	factorization13, factorization14, factorizationSummary,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回はリストを引数に取る再帰関数について学んだ", \t -> do
	text t "* 今回はリストを返す再帰関数について学ぶ", \t -> do
	text t "* 「リスト」の回でやった3段階は", \t -> do
	itext t 1 "1. enumerate", \t -> do
	itext t 1 "2. map/filter", \t -> do
	itext t 1 "3. accumulate", \t -> do
	text t "* そのうちの1. enumerateにあたる部分となる"
 ]

enumerate :: Page
enumerate = [\t -> do
	writeTopTitle t "[m .. n]"
	text t "", \t -> do
	text t "* まずは「リスト」の回で見た[m .. n]という構文を見よう", \t -> do
	text t "* これは構文糖であり以下の関数に変換される", \t -> do
	itext t 1 "enumFromTo m n", \t -> do
	text t "* この関数を正しく理解するには型クラスの知識が必要なので", \t -> do
	text t "* ここではこの関数の型を以下のように考える", \t -> do
	itext t 1 "enumFromTo :: Int -> Int -> [Int]"
 ]

aboutEnumFromTo :: Page
aboutEnumFromTo = [\t -> do
	writeTopTitle t "enumFromTo"
	text t "", \t -> do
	text t "* enumFromTo m nはmから1刻みでnまでのリストを返す関数", \t -> do
	text t "* 以下のように定義することができる", \t -> do
	itext t 1 "enumFromTo m n", \t -> do
	itext t 2 "| m > n = []", \t -> do
	itext t 2 "| otherwise = m : enumFromTo (m + 1) n"
 ]

myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo m n
	| m > n = []
	| otherwise = m : myEnumFromTo (m + 1) n

aboutEnumFromTo2 :: Page
aboutEnumFromTo2 = [\t -> do
	writeTopTitle t "試してみよう"
	text t "", \t -> do
	text t "* コマンドプロンプトを2こ立ち上げて", \t -> do
	text t "* lectures/lecture08ディレクトリを作成しそこに移動", \t -> do
	text t "* myList.hsを作成し以下を書き込もう", \t -> do
	itext t 1 "myEnumFromTo :: Int -> Int -> [Int]", \t -> do
	itext t 1 "myEnumFromTo m n", \t -> do
	itext t 2 "| m > n = []", \t -> do
	itext t 2 "| otherwise = m : myEnumFromTo (m + 1) n", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "% ghci myList.hs", \t -> do
	itext t 1 "*Main> myEnumFromTo 3 8", \t -> do
	itext t 1 $ show $ myEnumFromTo 3 8
 ]

aboutEnumFromTo3 :: Page
aboutEnumFromTo3 = [\t -> do
	writeTopTitle t "myEnumFromToの説明"
	text t "", \t -> do
	text t "* 定義を再掲する", \t -> do
	itext t 1 "myEnumFromTo m n"
	itext t 2 "| m > n = []"
	itext t 2 "| otherwise = m : enumFromTo (m + 1) n", \t -> do
	text t "* これはこう読める", \t -> do
	itext t 1 "1. 開始の値が終了の値より大きければ空リスト", \t -> do
	itext t 1 "2. mからnまでの値のリストは", \t -> do
	itext t 2 "(m + 1)からnまでの値のリストにmを追加したもの"
 ]

aboutEnumFromToSummary :: Page
aboutEnumFromToSummary = [\t -> do
	writeTopTitle t "enumFromTo(まとめ)"
	text t "", \t -> do
	text t "* [m .. n]という構文は構文糖であり", \t -> do
	arrowIText t 1 "enumFromTo m n", \t -> do
	text t "* enumFromTo m nは以下のように定義される", \t -> do
	itext t 1 "1. m > nならば空リスト", \t -> do
	itext t 1 "2. それ以外でm + 1からnのリストにmを追加したもの", \t -> do
	text t "* リストを作成する関数の多くが同様の枠組みで作られる", \t -> do
	itext t 1 "- その関数自体の返り値に値を追加するという枠組み"
 ]

collatz1 :: Page
collatz1 = [\t -> do
	writeTopTitle t "コラッツの予想"
	text t "", \t -> do
	text t "* 正の整数nについて", \t -> do
	itext t 1 "- nが偶数ならば2で割り", \t -> do
	itext t 1 "- nが奇数ならば3をかけて1を足す", \t -> do
	text t "* これをくりかえし行うと、", \t -> do
	text t "* いつかは4 -> 2 -> 1 -> 4 -> 2 -> 1のループになる", \t -> do
	text t "* という予想", \t -> do
	text t "* まだ証明されていない", \t -> do
	text t "* 数学における未解決問題のひとつ", \t -> do
	text t "* 現在では3 * 2 ^ 53までのnで成り立つことが確認ずみ", \t -> do
	itext t 1 $ "3 * 2 ^ 53 = " ++ show (3 * 2 ^ (53 :: Int) :: Integer)
--	itext t (-1) $ "二京七千二十一兆五千九百七十七億六千四百二十二万二千九百七十六"
 ]

collatz2 :: Page
collatz2 = [\t -> do
	writeTopTitle t "コラッツの予想"
	text t "", \t -> do
	text t "* コラッツ数列というものを考える", \t -> do
	itext t 1 "- 数nから始めて既述のルールで次を求めていき", \t -> do
	itext t 1 "- 1が来たら終了とする数列", \t -> do
	text t "* コラッツ数列を求める関数を書いていこう", \t -> do
	text t "* この関数は2つの部分に分けることができる", \t -> do
	itext t 1 "1. 既述のルールを適用し続ける無限リストを作る", \t -> do
	itext t 1 "2. 上記のリストのうち最初に1が出るところまでとる", \t -> do
	text t "* 「くりかえし」の部分と終了条件とを分けた"
 ]

collatz3 :: Page
collatz3 = [\t -> do
	writeTopTitle t "最初の1までをとる"
	text t "", \t -> do
	text t "* 引数で指定された条件を満たすまでをとる関数", \t -> do
	text t "* この関数をtakeToとしよう", \t -> do
	text t "* この関数の入出力の型は", \t -> do
	itext t 1 "- 入力1: a -> Bool", \t -> do
	itext t 1 "- 入力2: [a]", \t -> do
	itext t 1 "- 出力: [a]", \t -> do
	arrowIText t 1 "takeTo :: (a -> Bool) -> [a] -> [a]", \t -> do
	text t "* 中身について考えていこう"
 ]

collatz4 :: Page
collatz4 = [\t -> do
	writeTopTitle t "takeTo"
	text t "", \t -> do
	text t "* 空リストの場合は結果も空リストとなるので", \t -> do
	itext t 1 "takeTo _ [] = []", \t -> do
	text t "* 残りのケースは", \t -> do
	itext t 1 "リストの先頭が条件を満たす場合と満たさない場合", \t -> do
	text t "* 条件を満たした場合、その要素の後ろは捨てるので", \t -> do
	itext t 1 "takeTo p (x : xs)"
	itext t 2 "| p x = [x]", \t -> do
	text t "* pを満たさない場合は", \t -> do
	itext t 1 "- xsからpを満たす要素までを取り", \t -> do
	itext t 1 "- そのリストにxを追加するので", \t -> do
	itext t 2 "| otherwise = x : takeTo p xs"
 ]

takeTo :: (a -> Bool) -> [a] -> [a]
takeTo _ [] = []
takeTo p (x : xs)
	| p x = [x]
	| otherwise = x : takeTo p xs

cl5lst1 :: [Int]
cl5lst1 = unsafePerformIO $ do
	sg <- newStdGen
	let	rs = randomRs (0, 10) sg
		(xs, y : ys) = splitAt 3 rs
	return $ map ((+ 1) . (* 2)) xs ++ [2 * y] ++ take 3 ys

-- [3, 7, 9, 4, 5, 2, 8]

collatz5 :: Page
collatz5 = [\t -> do
	writeTopTitle t "takeTo"
	text t "", \t -> do
	text t "* collatz.hsに書き込もう", \t -> do
	itext t 1 "takeTo :: (a -> Bool) -> [a] -> [a]"
	itext t 1 "takeTo _ [] = []"
	itext t 1 "takeTo p (x : xs)"
	itext t 2 "| p x = [x]"
	itext t 2 "| otherwise = x : takeTo p xs", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :load collatz.hs", \t -> do
	itext t 1 $ "*Main> takeTo even " ++ show cl5lst1, \t -> do
	itext t 1 $ show $ takeTo even cl5lst1
 ]

collatz6 :: Page
collatz6 = [\t -> do
	writeTopTitle t "前回の復習"
	text t "", \t -> do
	text t "* 定義をもう一度見てみよう", \t -> do
	itext t 1 "takeTo _ [] = []"
	itext t 1 "takeTo p (x : xs)"
	itext t 2 "| p x = [x]"
	itext t 2 "| otherwise = x : takeTo p xs", \t -> do
	text t "* 以下のような関数を考える", \t -> do
	itext t 1 "fun x lst = if p x then [x] else x : lst", \t -> do
	text t "* すると", \t -> do
	itext t 1 "takeTo p (x : xs) = fun x (takeTo p xs)", \t -> do
	arrowIText t 1 "takeTo p (x : xs) = x `fun` takeTo p xs"
 ]

collatz7 :: Page
collatz7 = [\t -> do
	writeTopTitle t "前回の復習"
	text t "", \t -> do
	text t "* つまり以下のようになる", \t -> do
	itext t 1 "(takeTo p) [] = []", \t -> do
	itext t 1 "(takeTo p) (x : xs) = x `fun` (takeTo p) xs", \t -> do
	text t "* (takeTo p)は以下の変換を行う関数と見ることができる", \t -> do
	itext t 1 "- []は[]にする", \t -> do
	itext t 1 "- (:)をfunに置き換える", \t -> do
	text t "* このパターンはfoldrそのものなので", \t -> do
	itext t 1 "takeTo p = foldr fun []", \t -> do
	text t "* fun x lst = if p x then [x] else x : lstなので", \t -> do
	itext t 1 "takeTo p = foldr"
	itext t 2 "(\\x lst -> if p x then [x] else x : lst) []"
 ]

collatz8 :: Page
collatz8 = [\t -> do
	writeTopTitle t "蛇足"
	text t "", \t -> do
	text t "* ちなみにfunを以下のように書き換えることができる", \t -> do
	itext t 1 "\\x lst -> if p x then [x] else x : lst", \t -> do
	arrowIText t 1 "\\x -> if p x then \\_ -> [x] else (x :)", \t -> do
	text t "* (\\_ -> c)は引数に関わらず一定の値を返す関数", \t -> do
	text t "* そのような関数を作る関数constがある", \t -> do
	itext t 1 "const c = \\_ -> c", \t -> do
	text t "* それを使って書き換えると", \t -> do
	arrowIText t 1 "\\x -> if p x then const [x] else (x :)"
 ]

collatz9 :: Page
collatz9 = [\t -> do
	writeTopTitle t "蛇足の説明"
	text t "", \t -> do
	text t "* 見くらべてみる", \t -> do
	itext t 1 "\\x lst -> if p x then [x] else x : lst", \t -> do
	itext t 1 "\\x -> if p x then const [x] else (x :)", \t -> do
	text t "* Haskellに慣れてくると後者のほうが「美しく」感じる", \t -> do
	text t "* lstという変数はfooでもbarでも何でも良い", \t -> do
	text t "* lstという名前を選んだのは恣意的な選択である", \t -> do
	text t "* よってそのような「恣意性」が減少する後者のほうが美しい", \t -> do
	text t "* 仮引数を使わないスタイルをポイントフリースタイルと呼ぶ", \t -> do
	text t "* ポイントフリースタイルを適度に使うとコードが引きしまる"
 ]

collatz10 :: Page
collatz10 = [\t -> do
	writeTopTitle t "重複をなくす"
	text t "", \t -> do
	text t "* さっきの定義をもう一度見てみよう", \t -> do
	itext t 1 "\\x -> if p x then const [x] else (x :)", \t -> do
	text t "* [x]は(x : [])なので(x :)が重複している", \t -> do
	text t "* 以下のようにすると重複をなくすことができる", \t -> do
	itext t 1 "\\x -> (x :) . if p x then const [] else id", \t -> do
	text t "* idは入力をそのまま出力とする関数", \t -> do
	itext t 1 "id x = x"
 ]

takeTo' :: (a -> Bool) -> [a] -> [a]
-- takeTo' p = flip foldr [] $ \x -> if p x then const [x] else (x :)
-- takeTo' p = flip foldr [] $ \x -> (x :) . if p x then const [] else id
-- takeTo' p = flip foldr [] $ \x -> ((:) x) . ((ifThenElse (const []) id) . p $ x)
-- takeTo' p = flip foldr [] $ \x -> (.) ((:) x) ((ifThenElse (const []) id) . p $ x)
-- takeTo' p = flip foldr [] $ (.) <$> (:) <*> ifThenElse (const []) id . p
-- takeTo' p = flip foldr [] $ ((.) <$> (:) <*>) $ ifThenElse (const []) id . p
-- takeTo' p = flip foldr [] $ ((.) <$> (:) <*>) $ ([id, const []] !!) . fromEnum . p
-- takeTo' p = flip foldr [] $ ((.) <$> (:) <*>) $ (.) (([id, const []] !!) . fromEnum) p
-- takeTo' = flip foldr [] . ((.) <$> (:) <*>) . (.) (([id, const []] !!) . fromEnum)
takeTo' = flip foldr [] . ((.) <$> (:) <*>) . ((([id, const []] !!) . fromEnum) .)

ifThenElse :: a -> a -> Bool -> a
ifThenElse t e b = if b then t else e

collatz11 :: Page
collatz11 = [\t -> do
	writeTopTitle t "試してみよう"
	text t "", \t -> do
	text t "* collatz.hsに書き込もう", \t -> do
	itext t 1 "takeTo' :: (a -> Bool) -> [a] -> [a]", \t -> do
	itext t 1 "takeTo' p = flip foldr [] $", \t -> do
	itext t 2 "\\x -> (x :) . if p x then const [] else id", \t -> do
	itext t 2 "-- flipは引数の順番を入れ換える関数", \t -> do
	itext t 2 "-- それと($)を使うことで()を減らしてみた", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> takeTo' even " ++ show cl5lst1, \t -> do
	itext t 1 $ show $ takeTo' even cl5lst1
 ]

collatz12 :: Page
collatz12 = [\t -> do
	writeTopTitle t "ポイントフリー狂"
	text t "", \t -> do
	text t "* takeToを完全にポイントフリースタイルで書くとこうなる", \t -> do
	itext t 1 "takeTo = flip foldr [] ."
	itext t 2 "((.) <$> (:) <*>) ."
	itext t 2 "((([id, const []] !!) . fromEnum) .)", \t -> do
	text t "* ポイントフリースタイルを「適度に」使うとわかりやすく", \t -> do
	text t "* 「適度に」使うとわかりやすく", \t -> do
	text t "* 「適度に」使おう"
 ]

collatz13 :: Page
collatz13 = [\t -> do
	writeTopTitle t "最初の1までをとる(まとめ)"
	text t "", \t -> do
	text t "* 条件を満たす最初の要素までをとる関数takeToを作った", \t -> do
	text t "* まずは再帰的な定義で書いてみた", \t -> do
	text t "* (:)を置き換える関数を考えることで", \t -> do
	itext t 1 "- foldrを使った定義に書き換えてみた", \t -> do
	text t "* ポイントフリースタイルについて触れた", \t -> do
	itext t 1 "- ポイントフリースタイルはコードから贅肉をおとす", \t -> do
	itext t 1 "- やりすぎるとパズルになる", \t -> do
	text t "* takeToを使うとリストから最初の1までをとる処理は", \t -> do
	itext t 1 "takeTo (== 1) [...]"
 ]

collatz14 :: Page
collatz14 = [\t -> do
	writeTopTitle t "前の値から次の値を作る"
	text t "", \t -> do
	text t "* ここからが本題", \t -> do
	text t "* コラッツ数列の作りかたを再掲する", \t -> do
	itext t 1 "- nが偶数ならば2で割り", \t -> do
	itext t 1 "- nが奇数ならば3をかけて1を足す", \t -> do
	text t "* 前の値から次の値を求める関数", \t -> do
	itext t 1 "collatzNext :: Int -> Int", \t -> do
	itext t 1 "collatzNext n", \t -> do
	itext t 2 "| even n = n `div` 2", \t -> do
	itext t 2 "| otherwise = n * 3 + 1"
 ]

collatz15 :: Page
collatz15 = [\t -> do
	writeTopTitle t "無限数列を作る"
	text t "", \t -> do
	text t "* 以下のようにして無限数列を作る", \t -> do
	itext t 1 "collatzInf :: Int -> [Int]", \t -> do
	itext t 1 "collatzInf n = n : collatzInf (collatzNext n)", \t -> do
	text t "* nから始まる数列はnの次から始まる数列にnを追加したもの", \t -> do
	text t "* 結果は以下のような数列になる", \t -> do
	itext t 0.5 "[n, collatzNext n, collatzNext (collatzNext n) ...]", \t -> do
	text t "* これを一般化すると", \t -> do
	itext t 0.5 "[x, f x, f (f x), f (f (f x)), f (f (f (f x))) ...]", \t -> do
	text t "* このような列はしばしば使われるので関数が用意されている", \t -> do
	text t "* 上の[x, f x, ...]はiterate f xで表すことができる"
 ]

collatz16 :: Page
collatz16 = [\t -> do
	writeTopTitle t "無限数列を作る"
	text t "", \t -> do
	text t "* iterateを使うと以下のようにできる", \t -> do
	itext t 1 "collatzInf = iterate collatzNext", \t -> do
	text t "* collatzNextを展開すると", \t -> do
	itext t 1 "collatzInf = iterate $ \\n -> if even n"
	itext t 2 "then n `div` 2"
	itext t 2 "else n * 3 + 1"
 ]

collatzInf :: Int -> [Int]
collatzInf = iterate $ \n -> if even n
	then n `div` 2
	else n * 3 + 1

cl17int1 :: Int
cl17int1 = unsafePerformIO $ randomRIO (5, 30)

collatz17 :: Page
collatz17 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* collatz.hsに書き込もう", \t -> do
	itext t 1 "collatzInf :: Int -> [Int]"
	itext t 1 "collatzInf = iterate $ \\n -> if even n"
	itext t 2 "then n `div` 2"
	itext t 2 "else n * 3 + 1", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> take 10 $ collatzInf " ++ show cl17int1, \t -> do
	itext t 1 $ show $ take 10 $ collatzInf cl17int1
 ]

collatz18 :: Page
collatz18 = [\t -> do
	writeTopTitle t "iterate"
	text t "", \t -> do
	text t "* ここでiterateの定義を見てみよう", \t -> do
	text t "* 型は以下のようになる", \t -> do
	itext t 1 "iterate :: (a -> a) -> a -> [a]", \t -> do
	text t "* 定義は以下のようになる", \t -> do
	itext t 1 "iterate f x = x : iterate f (f x)", \t -> do
	text t "* xに次々とfを適用していってできるリストは", \t -> do
	itext t 1 "- (f x)に次々とfを適用していってできるリストに", \t -> do
	itext t 1 "- xを追加したもの"
 ]

collatz19 :: Page
collatz19 = [\t -> do
	writeTopTitle t "無限数列(まとめ)"
	text t "", \t -> do
	text t "* 次の値が前の値に関数fを適用したものであるようなリスト", \t -> do
	text t "* そのようなリストは以下のようにして作ることができる", \t -> do
	itext t 1 "mkSomeList x = x : mkSomeList (f x)", \t -> do
	text t "* この枠組みを関数として取り出したものがiterate関数", \t -> do
	itext t 1 "iterate f x", \t -> do
	arrowIText t 1 "[x, f x, f (f x), f (f (f x)) ...]"
 ]

collatz :: Int -> [Int]
collatz = takeTo (== 1) . collatzInf

collatz20 :: Page
collatz20 = [\t -> do
	writeTopTitle t "コラッツ数列"
	text t "", \t -> do
	text t "* 1で終了するようにしよう", \t -> do
	itext t 1 "collatz :: Int -> [Int]", \t -> do
	itext t 1 "collatz = takeTo (== 1) . collatzInf", \t -> do
	text t "* これをcollatz.hsに書き込もう", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> collatz 11", \t -> do
	itext t 1 $ show $ collatz 11
 ]

collatzSummary :: Page
collatzSummary = [\t -> do
	writeTopTitle t "コラッツ数列(まとめ)"
	text t "", \t -> do
	text t "* コラッツ数列を返す関数を作った", \t -> do
	text t "* 無限数列を作ってから必要な部分を取り出した", \t -> do
	itext t 1 "- 「次々に値を生成する」部分と", \t -> do
	itext t 1 "- 「終了条件」とを分けることができた", \t -> do
	text t "* 条件を満たすまでを取り出す関数を作った", \t -> do
	itext t 1 "- 復習のためにfoldrで書き直した", \t -> do
	itext t 1 "- ポイントフリースタイルの深渕をのぞいた", \t -> do
	text t "* 「値に次々と関数を適用する」という枠組みについて学んだ", \t -> do
	itext t 1 "- その枠組みは関数iterateによって抽象化されている"
 ]

factorization1 :: Page
factorization1 = [\t -> do
	writeTopTitle t "素因数分解"
	text t "", \t -> do
	text t "* 素因数分解の例について見ていこう", \t -> do
	text t "* 素因数分解とは数を素数の積で表現すること", \t -> do
	itext t 1 "420 = 2 * 2 * 3 * 5 * 7", \t -> do
	text t "* 素因数分解は桁が大きくなると計算量が増加する", \t -> do
	text t "* 大きな値を素因数分解する効率的なアルゴリズムはない", \t -> do
	arrowIText t 1 "むしろ、それを利用したのがRSA暗号"
 ]

factorization2 :: Page
factorization2 = [\t -> do
	writeTopTitle t "Int型"
	text t "", \t -> do
	text t "* あまり大きな素数は扱わない", \t -> do
	text t "* ある程度の大きさの合成数は扱う", \t -> do
	text t "* 今までは整数としてInt型の値を使ってきたが", \t -> do
	itext t 1 "- Intは処理系依存のサイズの整数型", \t -> do
	itext t 1 "- 仕様ではすくなくとも以下の範囲が表現可能とされる", \t -> do
	itext t 2 "- 2 ^ 29から2 ^ 29 - 1", \t -> do
	text t "* 自分の処理系のInt型の値の範囲は以下のように確かめられる", \t -> do
	itext t 1 "*Main> (minBound, maxBound) :: (Int, Int)", \t -> do
	itext t 1 $ show $ ((minBound, maxBound) :: (Int, Int))
 ]

maxInt :: Int
maxInt = maxBound

factorization3 :: Page
factorization3 = [\t -> do
	writeTopTitle t "Int型"
	text t "", \t -> do
	text t "* 実はInt型は大きな値に対しては安全ではない", \t -> do
	itext t 1 "*Main> maxBound :: Int", \t -> do
	itext t 1 $ show $ (maxBound :: Int), \t -> do
	itext t 1 "*Main> maxBound + 1 :: Int", \t -> do
	itext t 1 $ show $ (maxBound + 1 :: Int), \t -> do
	text t $ "* " ++ show maxInt ++ " + " ++ show (1 :: Int) ++ " = " ++ show (maxInt + 1) ++ " ???", \t -> do
	text t "* Int型はmaxBoundとminBoundとがつながった円環構造", \t -> do
	text t "* そういうものとして考える必要がある"
 ]

factorization4 :: Page
factorization4 = [\t -> do
	writeTopTitle t "Integer型"
	text t "", \t -> do
	text t "* Haskellにはより安全なInteger型がある", \t -> do
	text t "* メモリが許すかぎりの大きさの整数を扱うことができる", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> 2 ^ 128", \t -> do
	itext t 1 $ show $ (2 :: Integer) ^ (128 :: Int), \t -> do
	itext t 1 "*Main> product [1 .. 100]", \t -> do
	mapM_ (itext t 1) $ unfoldr (\str -> case str of
			[] -> Nothing
			_ -> Just $ splitAt 40 str) $
		show $ product [1 :: Integer .. 100], \t -> do
	text t "* 今回の例にはInteger型の値を使うことにする"
 ]

factorization5 :: Page
factorization5 = [\t -> do
	writeTopTitle t "戦略"
	text t "", \t -> do
	text t "* 問題を以下のように分割する", \t -> do
	itext t 1 "1. 整数を2以上の最小の約数と残りの数に分ける", \t -> do
	itext t 1 "2. 1を残りの数についてくりかえす", \t -> do
	text t "* まずは1.の整数を分ける関数について見ていこう"
 ]

factorization6 :: Page
factorization6 = [\t -> do
	writeTopTitle t "整数を分ける"
	text t "", \t -> do
	text t "* 効率のことは考えずに単純な実装とする", \t -> do
	text t "* nを引数として以下を求める関数を作る", \t -> do
	itext t 1 "1. 2から数nまでのリストを作る", \t -> do
	itext t 1 "2. それらでnを割り0になるものだけを集める", \t -> do
	itext t 1 "3. その先頭の要素をとる", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 0 "popFactor :: Integer -> Maybe (Integer, Integer)", \t -> do
	itext t 0 "popFactor 1 = Nothing", \t -> do
	itext t 0 "popFactor n = Just (f, n `div` f)", \t -> do
	itext t 1 "where"
	itext t 1 "f = head $ filter ((== 0) . (n `mod`)) [2 .. n]"
 ]

factorization7 :: Page
factorization7 = [\t -> do
	writeTopTitle t "整数を分割する"
	text t "", \t -> do
	text t "* 返り値をMaybeとしたことについて", \t -> do
	itext t 1 "- popFactor 1 = (1, 1)とすることもできたが", \t -> do
	itext t 1 "- 「残り」が変化しなくなると無限ループになる", \t -> do
	itext t 1 "- よってNothingとして終了条件を明確にした", \t -> do
	text t "* filterの部分を説明しよう", \t -> do
	itext t 1 "filter ((== 0) . (n `mod`))", \t -> do
	text t "* これはポイントフリーでないスタイルで書くとこうなる", \t -> do
	itext t 1 "filter (\\x -> n `mod` x == 0)", \t -> do
	text t "* つまりnを割った余りが0になるxを取り出すということ"
 ]

popFactor :: Integer -> Maybe (Integer, Integer)
popFactor 1 = Nothing
popFactor n = Just (f, n `div` f)
	where
	f = head $ filter ((== 0) . (n `mod`)) [2 .. n]

fct8integer1, fct8integer2 :: Integer
fct8integer1 = unsafePerformIO $ randomRIO (20, 100)
fct8integer2 = unsafePerformIO $ (+ 1) . (* 6) <$> randomRIO (5, 20)

factorization8 :: Page
factorization8 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* factorization.hsに以下を書き込もう", \t -> do
	itext t 0 "popFactor :: Integer -> Maybe (Integer, Integer)", \t -> do
	itext t 0 "popFactor n = (f, n `div` f)", \t -> do
	itext t 1 "where", \t -> do
	itext t 1 "f = head $ filter ((== 0) . (n `mod`)) [2 .. n]", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 $ "*Main> :load factorization.hs", \t -> do
	itext t 1 $ "*Main> popFactor " ++ show fct8integer1, \t -> do
	itext t 1 $ show $ popFactor fct8integer1, \t -> do
	itext t 1 $ "*Main> popFactor " ++ show fct8integer2, \t -> do
	itext t 1 $ show $ popFactor fct8integer2
 ]

factorization9 :: Page
factorization9 = [\t -> do
	writeTopTitle t "素因数分解"
	text t "", \t -> do
	text t "* 最小の因数と残りに分ける関数ができた", \t -> do
	text t "* これを使って素因数分解するためには", \t -> do
	itext t 1 "- 結果がNothingだったら空リスト", \t -> do
	itext t 1 "- そうでなければJust (f, n')として", \t -> do
	itext t 2 "n'の素因数分解の結果にfを追加すれば良い", \t -> do
	text t "* よって以下のようになる", \t -> do
	itext t 1 "factorization :: Integer -> [Integer]", \t -> do
	itext t 1 "factorization n = case popFactor n of", \t -> do
	itext t 2 "Nothing -> []", \t -> do
	itext t 2 "Just (f, n') -> f : factorization n'"
 ]

factorization10 :: Page
factorization10 = [\t -> do
	writeTopTitle t "iterateの枠組みとの比較"
	text t "", \t -> do
	text t "* iterateの場合", \t -> do
	itext t 1 "- 次々にわたしていく値と", \t -> do
	itext t 1 "- 結果としてリストに入る値とは同じ", \t -> do
	text t "* collatz数列の例で考えると", \t -> do
	itext t 1 "- 5の次の値は16だが、この16は", \t -> do
	itext t 1 "- 結果のリストに入り、かつ", \t -> do
	itext t 1 "- 残りの値を求めるのに使われる"
 ]

factorization11 :: Page
factorization11 = [\t -> do
	writeTopTitle t "iterateの枠組みとの比較"
	text t "", \t -> do
	text t "* 素因数分解の例では", \t -> do
	itext t 1 "- リストに入るのは最小の約数", \t -> do
	itext t 1 "- 次の値に必要なのは、残りの数", \t -> do
	itext t 1 "- リストの終了はNothingで示される", \t -> do
	text t "* この枠組みを表す関数unfoldrがある"
 ]

factorization12 :: Page
factorization12 = [\t -> do
	writeTopTitle t "unfoldr"
	text t "", \t -> do
	text t "* factorizationの定義を再掲する", \t -> do
	itext t 1 "factorization n = case popFactor n of"
	itext t 2 "Nothing -> []"
	itext t 2 "Just (f, n') -> f : factorization n'", \t -> do
	text t "* より一般的に書くと", \t -> do
	itext t 1 "fun x = case next x of"
	itext t 2 "Nothing -> []"
	itext t 2 "Just (r, x') -> r : fun x'", \t -> do
	text t "* nextの部分を変えていろいろな関数が作れる", \t -> do
	text t "* unfoldr nextでこれを表現できる"
 ]

factorization13 :: Page
factorization13 = [\t -> do
	writeTopTitle t "unfoldr"
	text t "", \t -> do
	text t "* unfoldrの定義は以下のようになる", \t -> do
	itext t 1 "unfoldr :: (b -> Maybe (a, b)) -> b -> [a]", \t -> do
	itext t 1 "unfoldr next x = case next x of", \t -> do
	itext t 2 "Nothing -> []", \t -> do
	itext t 2 "Just (r, x') -> r : unfoldr next x'", \t -> do
	text t "* unfoldrを使うとfactorizationは以下のようになる", \t -> do
	itext t 1 "factorization = unfoldr popFactor"
 ]

factorization :: Integer -> [Integer]
factorization = unfoldr popFactor

fct14integer1, fct14integer2 :: Integer
fct14integer1 = 3511932343773
fct14integer2 = 781923541387

factorization14 :: Page
factorization14 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* factorization.hsに書き込もう", \t -> do
	itext t 1 "factorization :: Integer -> [Integer]"
	itext t 1 "factorization = unfoldr popFactor", \t -> do
	text t "* factorization.hsの先頭にimport Data.List (unfoldr)が必要", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> factorization " ++ show fct14integer1, \t -> do
	itext t 1 $ show $ factorization fct14integer1, \t -> do
	itext t 1 $ "*Main> factorization " ++ show fct14integer2, \t -> do
	itext t 1 $ show $ factorization fct14integer2, \t -> do
	text t "* 適当な値を入れると評価に時間がかかりすぎる場合がある"
 ]

factorizationSummary :: Page
factorizationSummary = [\t -> do
	writeTopTitle t "素因数分解(まとめ)"
	text t "", \t -> do
	text t "* 素因数分解する関数を定義した", \t -> do
	text t "* unfoldrを利用した", \t -> do
	text t "* unfoldrは以下のような関数を引数としてとる", \t -> do
	itext t 1 "- 次が存在しないことをNothingで表す", \t -> do
	itext t 1 "- 値に対して(結果, 次の値)のペアを返す", \t -> do
	text t "* unfoldrは「結果の値」を要素とするリストを返す"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* リストを構成する再帰関数について見た", \t -> do
	text t "* その関数の返り値自体に値を追加するという定義", \t -> do
	text t "* そのような関数の多くは以下のような構造を持つ", \t -> do
	itext t 1 "[x, f x, f (f x), f (f (f x)) ... ]", \t -> do
	text t "* このような構造はiterate f xで表現できる", \t -> do
	text t "* 「結果」と「次の値」が違う場合iterateは使えない", \t -> do
	text t "* このような場合にはunfoldrが使える", \t -> do
	text t "* unfoldrは以下の関数を引数としてとる", \t -> do
	itext t 1 "- 次の値が存在しない場合はNothing", \t -> do
	itext t 1 "- 存在するなら(結果, 次の値)ペアを返す"
 ]
