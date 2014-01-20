import Lecture

subtitle :: String
subtitle = "第10回 演習(2日目)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	basic, basic2, basic3, basic4, basic5, basic6, basic7,
	defineMap, defineMap2, defineMap3, defineMap4, defineMap5
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 今日は再帰関数について学んだ", \t -> do
	text t "* Haskellにおいてリストは重要なデータ構造である", \t -> do
	text t "* リストを扱う多くの関数が用意されている", \t -> do
	text t "* それらの関数も再帰的に定義されている", \t -> do
	text t "* リストを扱う関数の使いかたや作られかたを見た", \t -> do
	text t "* 演習ではいろいろな再帰関数を定義していく", \t -> do
	text t "* とくにリストを扱う関数をいろいろなやりかたで定義する"
 ]

basic :: Page
basic = [\t -> do
	writeTopTitle t "まずは基本から"
	text t "", \t -> do
	text t "* 2のn乗を求める関数exp2を求める", \t -> do
	text t "* (^)が使えないと仮定する", \t -> do
	text t "* 演習10-1. Intを扱うとして型を求めよ", \t -> do
	itext t 1 "(30秒)", \t -> do
	text t "* 答え: exp2 :: Int -> Int", \t -> do
	text t "* 演習10-2. 再帰を直接使って定義せよ", \t -> do
	itext t 1 "(1分)"
 ]

basic2 :: Page
basic2 = [\t -> do
	writeTopTitle t "まずは基本から"
	text t "", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "exp2 0 = 1", \t -> do
	itext t 1 "exp2 n = 2 * exp2 (n - 1)", \t -> do
	text t "* 今までの説明のなかでは無視してきたことがある", \t -> do
	text t "* 上の定義には実は問題がある", \t -> do
	text t "* exp2に負の値を与えた場合に無限ループになってしまう", \t -> do
	itext t 1 "- 本当はInt型だと無限ループにはならないが", \t -> do
	text t "* 今までの講義のなかでも同様の問題のある関数を作ってきた", \t -> do
	text t "* 解決策は3つある", \t -> do
	itext t 1 "1. Maybe型を使う", \t -> do
	itext t 1 "2. 明示的なエラーを返す", \t -> do
	itext t 1 "3. 適当な値を返す"
 ]

basic3 :: Page
basic3 = [\t -> do
	writeTopTitle t "負の値の扱い"
	text t "", \t -> do
	text t "* 1が最も良い方法であり3が最も悪い方法である", \t -> do
	itext t 1 "1. Maybe型を使った場合", \t -> do
	itext t 2 "exp2 :: Int -> Maybe Int", \t -> do
	itext t 2 "exp2 n | n < 0 = Nothing", \t -> do
	itext t 2 "exp2 0 = Just 1", \t -> do
	itext t 2 "exp2 n = case exp2 (n - 1) of", \t -> do
	itext t 3 "Just r -> Just $ 2 * r", \t -> do
	itext t 3 "_ -> Nothing"
 ]

basic4 :: Page
basic4 = [\t -> do
	writeTopTitle t "負の値の扱い"
	text t "", \t -> do
	itext t 1 "2. 明示的なエラーを返す", \t -> do
	itext t 2 "exp2 :: Int -> Int", \t -> do
	itext t 2 "exp2 n | n < 0 = error \"Negative exponent\"", \t -> do
	itext t 2 "exp2 0 = 1", \t -> do
	itext t 2 "exp2 n = 2 * exp2 (n - 1)", \t -> do
	itext t 1 "3. 適当な値を返す", \t -> do
	itext t 2 "exp2 :: Int -> Int", \t -> do
	itext t 2 "exp2 n | n <= 0 = 1", \t -> do
	itext t 2 "exp2 n = 2 * exp2 (n - 1)", \t -> do
	text t "* そもそも負の値を含まない型を使うという手もある", \t -> do
	itext t 1 "exp2 :: Word -> Word"
 ]

basic5 :: Page
basic5 = [\t -> do
	writeTopTitle t "リストを使った解"
	text t "", \t -> do
	text t "* 無限リストを使うこともできる", \t -> do
	text t "* 以下のようなリストがあれば", \t -> do
	itext t 1 "exp2s = [1, 2, 4, 8, 16, 32 ...]", \t -> do
	text t "* exp2は以下のように定義できる", \t -> do
	itext t 1 "exp2 n = exp2s !! n", \t -> do
	text t "* 演習10-3. 無限リストexp2sを定義せよ", \t -> do
	itext t 1 "ヒント: mapまたはzipWithが使える", \t -> do
	itext t 1 "(1分)"
 ]

basic6 :: Page
basic6 = [\t -> do
	writeTopTitle t "リストを使った解"
	text t "", \t -> do
	text t "* すこし難しかったかもしれない", \t -> do
	text t "* 答えは以下のようになる", \t -> do
	itext t 1 "exp2s = 1 : map (* 2) exp2s", \t -> do
	text t "* zipWithを使って次のようにしても良い", \t -> do
	itext t 1 "exp2s = 1 : zipWith (+) exp2s exp2s"
 ]

basic7 :: Page
basic7 = [\t -> do
	writeTopTitle t "リストに対する再帰関数"
	text t "", \t -> do
	text t "* リストの要素のうちの奇数の総和を求める関数oddSum", \t -> do
	text t "* 演習10-4. sum, filterを使ってoddSumを定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* できただろうか?", \t -> do
	itext t 1 "oddSum = sum . filter odd", \t -> do
	text t "* 演習10-5. 再帰的定義を直接使ってoddSumを定義せよ", \t -> do
	itext t 1 "(30秒)", \t -> do
	text t "* できただろうか?", \t -> do
	itext t 1 "oddSum [] = 0", \t -> do
	itext t 1 "oddSum (n : ns)", \t -> do
	itext t 2 "| odd n = n + oddSum ns", \t -> do
	itext t 2 "| otherwise = oddSum ns"
 ]

defineMap :: Page
defineMap = [\t -> do
	writeTopTitle t "map"
	text t "", \t -> do
	text t "* 演習10-6. 再帰的定義を直接使ってmapを定義せよ", \t -> do
	itext t 1 "(2分)", \t -> do
	text t "* できただろうか?", \t -> do
	itext t 1 "map :: (a -> b) -> [a] -> [b]", \t -> do
	itext t 1 "map f [] = []", \t -> do
	itext t 1 "map f (x : xs) = f x : map f xs", \t -> do
	text t "* 以下のように考える", \t -> do
	itext t 1 "- 空リストのすべての要素にfを適用しても空リスト", \t -> do
	itext t 1 "- (x : xs)のすべての要素にfを適用したものは", \t -> do
	itext t 2 "xにfを適用したものを"
	itext t 2 "xsの要素すべてにfを適用したものに加えたもの"
 ]

defineMap2 :: Page
defineMap2 = [\t -> do
	writeTopTitle t "map"
	text t "", \t -> do
	text t "* 演習10-7. foldrを使ってmapを定義せよ(2分)", \t -> do
	text t "* すこし難しかったかもしれない", \t -> do
	text t "* (:)を置き換える「演算子」を考えれば良い", \t -> do
	text t "* map f (x : xs) = f x : map f xsをこう書いてみる", \t -> do
	itext t 1 "map f (x : xs) = x `op` map f xs", \t -> do
	text t "* すると以下のようになる", \t -> do
	itext t 1 "x `op` lst = f x : lst", \t -> do
	arrowIText t 1 "op = \\x lst -> f x : lst", \t -> do
	arrowIText t 1 "op = \\x -> (f x :)", \t -> do
	arrowIText t 1 "op = \\x -> (:) f x", \t -> do
	arrowIText t 1 "op = (:) . f"
 ]

defineMap3 :: Page
defineMap3 = [\t -> do
	writeTopTitle t "map"
	text t "", \t -> do
	text t "* よってこうなる", \t -> do
	itext t 1 "map f = foldr ((:) . f) []"
 ]

defineMap4 :: Page
defineMap4 = [\t -> do
	writeTopTitle t "map"
	text t "", \t -> do
	text t "* foldrを使ってmapを定義した", \t -> do
	text t "* mapをリストを引数とする再帰関数として見た", \t -> do
	text t "* しかしmapはリストを返す関数でもある", \t -> do
	text t "* 演習10-8. mapをunfoldrを使って定義せよ", \t -> do
	itext t 1 "(2分)"
 ]

defineMap5 :: Page
defineMap5 = [\t -> do
	writeTopTitle t "map"
	text t "", \t -> do
	text t "* これも難しかったかもしれない", \t -> do
	text t "* 以下の3つを考えれば答えが出る", \t -> do
	itext t 1 "終了条件: []になれば終了", \t -> do
	itext t 1 "(x : xs)に対して", \t -> do
	itext t 2 "結果のリストに含まれる値: f x", \t -> do
	itext t 2 "次の計算にわたす値: xs", \t -> do
	text t "* 頭をけずったリストを次々にわたしながら", \t -> do
	text t "* 頭の値にfを適用したものを集める関数", \t -> do
	itext t 1 "map f = unfoldr $ \\lst -> case lst of", \t -> do
	itext t 2 "[] -> Nothing", \t -> do
	itext t 2 "(x : xs) -> Just (f x, xs)"
 ]
