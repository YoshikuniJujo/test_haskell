import Lecture

subtitle :: String
subtitle = "第10回 演習(2日目)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2, prelude3,
	basic, basic2, basic3, basic4, basic5, basic6, basic7,
	defineMap, defineMap2, defineMap3, defineMap4, defineMap5,
	defineFilter, defineFilter2, defineFilter3, defineFilter4,
	defineReverse, defineReverse2, defineReverse3, defineReverse4,
	defineReverse5,
	defineZip, defineZip2, defineZip3, defineZip4,
	defineUnzip, defineUnzip2, defineUnzip3,
	defineZipWith, defineZipWith2, defineZipWith3, defineZipWith4,
	summary
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

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "そのまえに"
	text t "", \t -> do
	text t "* importしなくても初めから使える関数等がある", \t -> do
	text t "* それらはPreludeというモジュールからexportされている", \t -> do
	text t "* ghciを引数無しで立ち上げたときはPreludeのなかにいる", \t -> do
	text t "* どのモジュールにも暗黙にimportされるので", \t -> do
	itext t 1 "同じ名前の別の関数を使いたいときに困る", \t -> do
	text t "* そのようなときは以下のように明示的にimportすれば良い", \t -> do
	itext t 1 "import Prelude hiding ([再定義したい関数名])"
 ]

prelude3 :: Page
prelude3 = [\t -> do
	writeTopTitle t "そのまえに"
	text t "", \t -> do
	text t "* 今回はPreludeの関数を再定義していくので", \t -> do
	itext t 1 "import Prelude hiding (map, filter ...)とする", \t -> do
	text t "* また、同じ関数にいろいろな定義を与えるので", \t -> do
	itext t 1 "* '--'を行頭につけてコメントアウトする", \t -> do
	itext t 1 "* '{-'と'-}'を使ったコメントも使える", \t -> do
	itext t 1 "* または、(')をつけてmap', map''のようにする"
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
	text t "* 演習10-5. 再帰を直接使ってoddSumを定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
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
	text t "* 演習10-6. 再帰を直接使ってmapを定義せよ", \t -> do
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
	text t "* (:)を置き換える「演算子op」を考えれば良い", \t -> do
	text t "* map f (x : xs) = f x : map f xsをこう書いてみる", \t -> do
	itext t 1 "map f (x : xs) = x `op` map f xs", \t -> do
	text t "* すると以下のようになる", \t -> do
	itext t 1 "x `op` lst == f x : lst", \t -> do
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
	itext t 2 "x : xs -> Just (f x, xs)"
 ]

defineFilter :: Page
defineFilter = [\t -> do
	writeTopTitle t "filter"
	text t "", \t -> do
	text t "演習10-9. filterを再帰を直接使って定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* こう考える", \t -> do
	itext t 1 "- 空リストからは何も選べないので空リスト", \t -> do
	itext t 1 "- 先頭の要素が条件を満たす場合", \t -> do
	itext t 2 "残りの要素をfilterしたものに先頭の要素を追加", \t -> do
	itext t 1 "- 満たさないときは残りの要素をfilterしたもの"
 ]

defineFilter2 :: Page
defineFilter2 = [\t -> do
	writeTopTitle t "filter"
	text t "", \t -> do
	text t "* よって以下のようになる", \t -> do
	itext t 1 "filter :: (a -> Bool) -> [a] -> [a]", \t -> do
	itext t 1 "filter _ [] = []", \t -> do
	itext t 1 "filter p (x : xs)", \t -> do
	itext t 2 "| p x = x : filter p xs", \t -> do
	itext t 2 "| otherwise = filter p xs"
 ]

defineFilter3 :: Page
defineFilter3 = [\t -> do
	writeTopTitle t "filter"
	text t "", \t -> do
	text t "* 演習10-10. filterをfoldrを使って定義せよ", \t -> do
	itext t 1 "(2分)", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* まずは空リストは空リストに変換される", \t -> do
	itext t 1 "filter _ [] = []"
 ]

defineFilter4 :: Page
defineFilter4 = [\t -> do
	writeTopTitle t "filter"
	text t "", \t -> do
	text t "* リストが空じゃない場合", \t -> do
	itext t 1 "filter p (x : xs)"
	itext t 2 "| p x = x : filter p xs"
	itext t 2 "| otherwise = filter p xs", \t -> do
	text t "* 以下の定義を満たすようなopを考える", \t -> do
	itext t 1 "filter p (x : xs) = x `op` filter p xs", \t -> do
	text t "* opは以下のようになる", \t -> do
	itext t 1 "op = \\x xs -> if p x then x : xs else xs", \t -> do
	arrowIText t 1 "op = \\x -> if p x then (x :) else id", \t -> do
	text t "* よってfoldrを使ったfilterの定義は", \t -> do
	text t "filter p = foldr (\\x -> if p x then (x :) else id) []"
 ]

defineReverse :: Page
defineReverse = [\t -> do
	writeTopTitle t "reverse"
	text t "", \t -> do
	text t "* リストを逆順にする関数reverseがある", \t -> do
	text t "* 再帰を直接使ってreverseを定義してみよう", \t -> do
	text t "* これは反復的処理を使うとスマートに定義できる", \t -> do
	text t "* リストの頭から要素を取っていき", \t -> do
	itext t 1 "次々とリストの頭に足していく", \t -> do
	text t "* トランプの山を一枚ずつ別の山に移動していくイメージ", \t -> do
	itext t 1 "- 結果としてできる山は逆順になっているはずだ"
 ]

defineReverse2 :: Page
defineReverse2 = [\t -> do
	writeTopTitle t "reverse"
	text t "", \t -> do
	text t "* 反復的処理には蓄積変数が必要となる", \t -> do
	text t "* これはreverseの引数とは別に用意してやる必要がある", \t -> do
	text t "* 別の関数を作り、その関数に蓄積変数の初期値を与える", \t -> do
	text t "* つまりreverseを以下のように定義する", \t -> do
	itext t 1 "reverse = reverseIter []"
 ]

defineReverse3 :: Page
defineReverse3 = [\t -> do
	writeTopTitle t "reverse"
	text t "", \t -> do
	text t "* 演習10-11. reverseIterの型を求めよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* できただろうか", \t -> do
	itext t 1 "第1引数(蓄積変数): 何かのリスト", \t -> do
	itext t 1 "第2引数(もとのリスト): 同じ型のリスト", \t -> do
	itext t 1 "返り値: 同じ型のリスト", \t -> do
	arrowIText t 1 "[a] -> [a] -> [a]"
 ]

defineReverse4 :: Page
defineReverse4 = [\t -> do
	writeTopTitle t "reverse"
	text t "", \t -> do
	text t "* 演習10-12. reverseIterを再帰を直接使って定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* reverseIter ys xsとしたとき", \t -> do
	itext t 1 "- xsが空ならばysをそのまま返せば良い", \t -> do
	itext t 1 "- そうでなければxsの先頭要素をysに移す", \t -> do
	itext t 1 "- これをくりかえす", \t -> do
	text t "* よって、こうなる", \t -> do
	itext t 1 "reverseIter ys [] = ys", \t -> do
	itext t 1 "reverseIter ys (x : xs) = reverseIter (x : ys) xs"
 ]

defineReverse5 :: Page
defineReverse5 = [\t -> do
	writeTopTitle t "reverse"
	text t "", \t -> do
	text t "* 演習10-13. reverseをfoldlを使って定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* foldlはこう考える", \t -> do
	itext t 1 "- 初期値は何か?", \t -> do
	itext t 1 "- リストの次の値によって蓄積変数はどう変わるか", \t -> do
	text t "* reverseだとこうなる", \t -> do
	itext t 1 "- 初期値: []", \t -> do
	itext t 1 "- 次の値xによって蓄積変数ysは(x : ys)となる", \t -> do
	arrowIText t 1 "reverse = foldl (\\ys x -> x : ys) []", \t -> do
	text t "* \\ys x -> x : ysは(:)の引数を入れ換えただけなので", \t -> do
	arrowIText t 1 "reverse = foldl (flip (:)) []"
 ]

defineZip :: Page
defineZip = [\t -> do
	writeTopTitle t "zip"
	text t "", \t -> do
	text t "* ふたつのリストをタプルのリストにまとめる関数zipがある", \t -> do
	itext t 1 "Prelude> zip \"hello\" [1 ..]", \t -> do
	itext t 1 $ show $ zip "hello" [1 :: Int ..], \t -> do
	text t "* 演習10-14. zipの型を決めよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* aの値のリストとbの値のリストをとって"
	itext t 1 "タプル(a, b)のリストを作るので", \t -> do
	arrowIText t 1 "[a] -> [b] -> [(a, b)]"
 ]

defineZip2 :: Page
defineZip2 = [\t -> do
	writeTopTitle t "zip"
	text t "", \t -> do
	text t "* 演習10-15. zipを再帰を直接使って定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* こう考える", \t -> do
	itext t 1 "- どちらかが空リストなら空リスト", \t -> do
	itext t 1 "- 両方のリストの残りのリストをzipしたものがあれば", \t -> do
	itext t 2 "それに両方のリストの頭のタプルを足す", \t -> do
	text t "* こうなる", \t -> do
	itext t 1 "zip [] _ = []"
	itext t 1 "zip _ [] = []"
	itext t 1 "zip (x : xs) (y : ys) = (x, y) : zip xs ys"
 ]

defineZip3 :: Page
defineZip3 = [\t -> do
	writeTopTitle t "zip"
	text t "", \t -> do
	text t "* 演習10-16. zipをunfoldrを使って定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* こう考える", \t -> do
	itext t 1 "- どちらかが空ならば空リスト", \t -> do
	itext t 1 "- 蓄積変数の値は2つのリストをタプルにしたもの", \t -> do
	itext t 1 "- それぞれのリストの頭をタプルにしたものが結果", \t -> do
	itext t 1 "- 2つのリストの頭をとったものを次の値にする", \t -> do
	text t "* よってunfoldrに与える関数は以下のようになる", \t -> do
	itext t 1 "f (x : xs, y : ys) = Just ((x, y), (xs, ys))", \t -> do
	itext t 1 "f _ = Nothing"
 ]

defineZip4 :: Page
defineZip4 = [\t -> do
	writeTopTitle t "zip"
	text t "", \t -> do
	text t "* fを変形する", \t -> do
	itext t 1 "f = \\lsts -> case lsts of"
	itext t 2 "(x : xs, y : ys) -> Just ((x, y), (xs, ys))"
	itext t 2 "_ -> Nothing", \t -> do
	text t "* これをunfoldrに与えると以下の型の関数ができる", \t -> do
	itext t 1 "([a], [b]) -> [(a, b)]", \t -> do
	text t "* これをカリー化すれば求める関数となる", \t -> do
	itext t 1 "zip = curry $ unfoldr $ \\lsts -> case lsts of"
	itext t 2 "(x : xs, y : ys) -> Just ((x, y), (xs, ys))"
	itext t 2 "_ -> Nothing"
 ]

defineUnzip :: Page
defineUnzip = [\t -> do
	writeTopTitle t "unzip"
	text t "", \t -> do
	text t "* zipの逆関数が存在する", \t -> do
	text t "* 2要素タプルを2つのリストに分ける関数", \t -> do
	text t "* 2つのリストはタプルにまとめて返される", \t -> do
	itext t 1 $ "Prelude> unzip " ++ show (zip "yes" [1 :: Int ..]), \t -> do
	itext t 1 $ show $ unzip $ zip "yes" [1 :: Int ..], \t -> do
	text t "* 演習10-17. unzipの型を求めよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* 入力がタプルのリストで出力が"
	itext t 1 "タプルのそれぞれの要素のリストのタプルなので", \t -> do
	arrowIText t 1 "[(a, b)] -> ([a], [b])"
 ]

defineUnzip2 :: Page
defineUnzip2 = [\t -> do
	writeTopTitle t "unzip"
	text t "", \t -> do
	text t "* 演習10-18. unzipを再帰を直接使って定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* こう考える", \t -> do
	itext t 1 "- 空リストなら空リストのペアになる", \t -> do
	itext t 1 "- リストのはじめの要素以外をunzipしたものがあれば", \t -> do
	itext t 2 "それぞれのリストにペアのそれぞれの値を追加", \t -> do
	text t "* こうなる", \t -> do
	itext t 1 "unzip [] = ([], [])", \t -> do
	itext t 1 "unzip ((x, y) : xys) = (x : xs, y : ys)", \t -> do
	itext t 2 "where (xs, ys) = unzip xys"
 ]

defineUnzip3 :: Page
defineUnzip3 = [\t -> do
	writeTopTitle t "unzip"
	text t "", \t -> do
	text t "* 演習10-19. unzipをfoldrを使って定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* 要素がひとつつけ加わったときにどう変化するかを考える", \t -> do
	text t "* (xs, ys)に対して(x, y)が加わると(x : xs, y: ys)になる", \t -> do
	text t "* よって以下のような定義となる", \t -> do
	itext t 1 "unzip = foldr $"
	itext t 2 "\\(x, y) (xs, ys) -> (x : xs, y : ys)) []"
 ]

defineZipWith :: Page
defineZipWith = [\t -> do
	writeTopTitle t "zipWith"
	text t "", \t -> do
	text t "* 2つのリストの要素同士を", \t -> do
	itext t 1 "- タプルにまとめるのではなく", \t -> do
	itext t 1 "- 任意の関数でまとめる関数がzipWith", \t -> do
	itext t 1 "Prelude> zipWith (+) [1, 2, 3] [4, 5, 6]", \t -> do
	itext t 1 $ show $ zipWith (+) [1 :: Int, 2, 3] [4, 5, 6], \t -> do
	text t "* 演習10-20. zipWithの型を求めよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* 第1引数は2引数関数で", \t -> do
	itext t 1 "- その関数の引数と返り値の型をリストにしたものが", \t -> do
	itext t 1 "- zipWith全体の残りの引数と返り値の型となる", \t -> do
	arrowIText t 1 "zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]"
 ]

defineZipWith2 :: Page
defineZipWith2 = [\t -> do
	writeTopTitle t "zipWith"
	text t "", \t -> do
	text t "* タプルは(x, y)で表記されるが本当は以下の形をしている", \t -> do
	itext t 1 "(,) x y", \t -> do
	text t "* つまり二引数関数(,)をx, yに適用すると(x, y)となる", \t -> do
	text t "* 演習10-21. zipWithを使ってzipを定義せよ", \t -> do
	itext t 1 "(30秒)", \t -> do
	text t "* zip = zipWith (,)"
 ]

defineZipWith3 :: Page
defineZipWith3 = [\t -> do
	writeTopTitle t "zipWith"
	text t "", \t -> do
	text t "* 演習10-22. zipWithを再帰を直接使って定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* こう考える", \t -> do
	itext t 1 "- どちらかが空リストなら空リスト", \t -> do
	itext t 1 "- xsとysについての結果zsがあるなら", \t -> do
	itext t 2 "(x : xs)と(y : ys)に対してはf x y : zs", \t -> do
	text t "* こうなる", \t -> do
	itext t 1 "zipWith _ [] _ = []"
	itext t 1 "zipWith _ _ [] = []"
	itext t 1 "zipWith f (x : xs) (y : ys) ="
	itext t 2 "f x y : zipWith f xs ys"
 ]

defineZipWith4 :: Page
defineZipWith4 = [\t -> do
	writeTopTitle t "zipWith"
	text t "", \t -> do
	text t "* 演習10-23. zipWithをunfoldrを使って定義せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* (x : xs, y : ys)に対して", \t -> do
	itext t 1 "- 結果のリストにはf x yが含まれ", \t -> do
	itext t 1 "- 次の計算には(xs, ys)がわたされる", \t -> do
	text t "* こうなる", \t -> do
	itext t 0 "zipWith f = curry $ unfoldr $ \\lsts -> case lsts of"
	itext t 1 "(x : xs, y : ys) -> Just (f x y, (xs, ys))"
	itext t 1 "_ -> Nothing"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* いろいろな再帰関数を定義してみた", \t -> do
	text t "* とくにリストを扱う再帰関数について", \t -> do
	itext t 1 "- 直接的な定義を作成した", \t -> do
	itext t 1 "- 抽象的な枠組みを抽出した関数を使って定義した", \t -> do
	text t "* 再帰的関数の定義のしかたを身につけた", \t -> do
	text t "* 直接的な再帰より狭い枠組みを抽出した関数を使うことで", \t -> do
	itext t 1 "定義しようとしている関数のパターンを見出した"
 ]
