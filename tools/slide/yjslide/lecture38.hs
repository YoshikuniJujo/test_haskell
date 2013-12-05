import Lecture

subtitle :: String
subtitle = "第38回 可変配列"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	eratosthenes, pseudoEratosthenes, eratosthenes2
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回は不変配列について学んだ", \t -> do
	text t "* 今回は可変配列について学ぶ", \t -> do
	text t "* 世のなかには多くのアルゴリズムが存在する", \t -> do
	text t "* その多くは効率的な解法を追及した結果できた", \t -> do
	text t "* 多くのものは状態変化を必要とする", \t -> do
	text t "* またランダムアクセスがO(1)である配列も必要だ", \t -> do
	text t "* より関数型的な解法があるアルゴリズムも多々ある", \t -> do
	text t "* 状態変化と配列を駆使したアルゴリズムのほうが簡単", \t -> do
	text t "* 状態変化を利用するためにはIOモナド内で使う必要がある", \t -> do
	text t "* より洗練されたSTモナドについては上級編で"
 ]

eratosthenes :: Page
eratosthenes = [\t -> do
	writeTopTitle t "エラトステネスの篩"
	text t "", \t -> do
	text t "* エラトステネスの篩というアルゴリズムがある", \t -> do
	text t "* 与えられた値xまでの素数を求めるアルゴリズム", \t -> do
	itext t 1 "1. 配列に2からxまでの整数を入れる", \t -> do
	itext t 1 "2. 2からxの2乗根以下の数まで3,4をくりかえす", \t -> do
	itext t 1 "3. その数にマークがついていたら何もしない", \t -> do
	itext t 1 "4. でなければその数の2倍以上の数にマークをつける", \t -> do
	itext t 1 "5. マークのついてない数が素数である"
 ]

pseudoEratosthenes :: Page
pseudoEratosthenes = [\t -> do
	writeTopTitle t "エラトステネスの篩的なもの"
	text t "", \t -> do
	text t "* 非常にきれいだが非効率的な実装", \t -> do
	itext t 1 "primes = sieve [2 ..]"
	itext t 1 "sieve (p : xs) ="
	itext t 2 "p : sieve [x | x <- xs, x `mod` p > 0]", \t -> do
	text t "メモ: 6, 7, 8, 9を全部5でわって確認している"
	itext t 1 "本当の篩であれば5でわれるものだけを相手にする"
 ]

eratosthenes2 :: Page
eratosthenes2 = [\t -> do
	writeTopTitle t "エラトステネスの篩"
	text t "", \t -> do
	text t "* これを実装していくなかで可変配列の使いかたを学ぼう", \t -> do
	text t "* IOArrayについて", \t -> do
	itext t 1 "IOArray i e", \t -> do
	text t "* 初期値を設定して配列を作成", \t -> do
	itext t 1 "newArray :: Ix i =>"
	itext t 2 "(i, i) -> e -> IO (IOArray i e)", \t -> do
	text t "* 配列への要素の書き込み", \t -> do
	itext t 1 "writeArray :: Ix i =>"
	itext t 2 "IOArray i e -> i -> e -> IO ()", \t -> do
	text t "* 配列からの要素の読み出し", \t -> do
	itext t 1 "readArray :: Ix i => IOArray i e -> i -> IO e"
 ]
