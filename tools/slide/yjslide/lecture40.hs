import Lecture

subtitle :: String
subtitle = "第39回 可変配列"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	eratosthenes, pseudoEratosthenes, eratosthenes2,
	eratosthenes3, eratosthenes4, eratosthenes5, eratosthenes6,
	eratosthenes7, eratosthenes8, eratosthenes9, marray
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
	text t "* これは実のところ篩と呼べる実装ではない", \t -> do
	text t "* 本当の篩は「倍数」だけを相手にしているが", \t -> do
	itext t 1 "- これは割り切れないものもチェックしている", \t -> do
	text t "* たとえば、7を5でわって確認している", \t -> do
	text t "* この実装で1万番目の素数を求めるのに4.77秒かかる"
 ]

eratosthenes2 :: Page
eratosthenes2 = [\t -> do
	writeTopTitle t "エラトステネスの篩"
	text t "", \t -> do
	text t "* 本物のエラトステネスの篩を実装するには可変配列が必要", \t -> do
	text t "* これを実装していくなかで可変配列の使いかたを学ぼう"
	text t "", \t -> do
	text t "* IOArrayについて", \t -> do
	itext t 1 "IOArray i e", \t -> do
	text t "* 初期値を設定して配列を作成", \t -> do
	itext t 1 "newArray :: Ix i =>"
	itext t 2 "(i, i) -> e -> IO (IOArray i e)"
 ]

eratosthenes3 :: Page
eratosthenes3 = [\t -> do
	writeTopTitle t "エラトステネスの篩"
	text t "", \t -> do
	text t "* 配列への要素の書き込み", \t -> do
	itext t 1 "writeArray :: Ix i =>"
	itext t 2 "IOArray i e -> i -> e -> IO ()", \t -> do
	text t "* 配列からの要素の読み出し", \t -> do
	itext t 1 "readArray :: Ix i => IOArray i e -> i -> IO e", \t -> do
	text t "* [(インデックス, 値)]形式への変換", \t -> do
	itext t 1 "getAssocs :: Ix i => IOArray i e -> IO [(i, e)]"
 ]

eratosthenes4 :: Page
eratosthenes4 = [\t -> do
	writeTopTitle t "エラトステネスの篩"
	text t "", \t -> do
	text t "* 実装は以下のようになる", \t -> do
	itext t 0 "sieve :: Int -> IO (IOArray Int Bool)"
	itext t 0 "sieve n = do"
	itext t 1 "arr <- newArray (2, n ^ 2) True"
	itext t 1 "forM_ [2 .. n] $ \\p -> do"
	itext t 2 "isPrime <- readArray arr p"
	itext t 2 "when isPrime $"
	itext t 3 "forM_ [2 * p, 3 * p .. n ^ 2] $ \\k ->"
	itext t 4 "writeArray arr k False"
	itext t 1 "return arr"
 ]

eratosthenes5 :: Page
eratosthenes5 = [\t -> do
	writeTopTitle t "エラトステネスの篩"
	text t "", \t -> do
	text t "* 素数のリストを取り出す", \t -> do
	itext t 1 "primesTo :: Int -> IO [Int]"
	itext t 1 "primesTo n = do"
	itext t 2 "arr <- sieve n >>= getAssocs"
	itext t 2 "return [fst p | p <- arr, snd p]", \t -> do
	text t "* テスト用のmain関数", \t -> do
	itext t 1 "main :: IO ()"
	itext t 1 "main = print =<< (!! 10000) <$> primesTo 324", \t -> do
	text t "* これにかかる時間が0.12秒", \t -> do
	itext t 1 "- 「的なもの」で4.77秒だったので40倍の速度が出た"
 ]

eratosthenes6 :: Page
eratosthenes6 = [\t -> do
	writeTopTitle t "エラトステネスの篩"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/primes/eratosthenes.png"), \t -> do
	text t "* メモリの使用状況", \t -> do
	itext t 1 "- 1.5MBほど使っている"
 ]

eratosthenes7 :: Page
eratosthenes7 = [\t -> do
	writeTopTitle t "エラトステネスの篩"
	text t "", \t -> do
--	text t "* IOArrayに対してIOUArrayというものがある", \t -> do
	text t "* Haskellでは遅延評価のためにbox化された型を使っている", \t -> do
	text t "* box化された型とは?", \t -> do
	itext t 1 "- 値そのものまたは評価前の式をその値として持つ型", \t -> do
	text t "* box化された型の値はそのぶんメモリを食う", \t -> do
	text t "* 今回のBool値は遅延評価する必要はないので", \t -> do
	itext t 1 "- unbox型の配列を使ったほうが空間効率が上がる", \t -> do
	text t "* そのための型としてIOUArrayがある", \t -> do
	itext t 1 "- 値として整数や実数などの限られた型のみ", \t -> do
	itext t 1 "- 真偽値もそのうちのひとつ"
 ]

eratosthenes8 :: Page
eratosthenes8 = [\t -> do
	writeTopTitle t "エラトステネスの篩"
	text t "", \t -> do
	text t "* IOUArrayを使って再定義してみる", \t -> do
	text t "* sieveの型宣言のみ変えれば良い", \t -> do
	itext t 1 "sieve :: Int -> IO (IOUArray Int Bool)", \t -> do
	text t "* メモリの使用量を見てみよう"
 ]

eratosthenes9 :: Page
eratosthenes9 = [\t -> do
	writeTopTitle t "エラトステネスの篩"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/primes/eratosthenesU.png"), \t -> do
	text t "* 1MBほどのメモリの使用量", \t -> do
	itext t 1 "- IOArray版の1.5MBと比べて2/3ほどとなっている"
 ]

marray :: Page
marray = [\t -> do
	writeTopTitle t "MArrayクラス"
	text t "", \t -> do
	text t "* IOUArray型に換えるときに型宣言だけ変えれば良かった", \t -> do
	itext t 1 "- IOArray型を扱う関数をいくつか紹介した", \t -> do
	itext t 1 "- 実際はMArrayクラスに対して定義されている", \t -> do
	text t "* MArrayクラスの定義", \t -> do
	itext t 1 "class Monad m => MArray a e m where"
	itext t 2 "..."
 ]
