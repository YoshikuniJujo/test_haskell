import Lecture

subtitle :: String
subtitle = "第27回 正格評価"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	strictEvaluation, spaceLeak,
	useFoldl, useFoldl2, useFoldl3, useFoldl4,
	aboutSeq,
	useSeq, useSeq2, useSeq3, aboutFoldl',
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 遅延評価のことを非正格評価とも呼ぶ", \t -> do
	text t "* 先行評価のことを正格評価と呼ぶ", \t -> do
	text t "* Haskellは非正格評価言語である", \t -> do
	itext t 1 "- 半面の真実", \t -> do
	text t "* Haskellは正格、非正格の両方を自由に選択できる言語", \t -> do
	text t "* デフォルトは非正格", \t -> do
	itext t 1 "- これは理にかなっている", \t -> do
	itext t 1 "- 評価前の値に評価を強制する", \t -> do
	arrowIText t 2 "簡単", \t -> do
	itext t 1 "- 逆は難しい"
 ]

strictEvaluation :: Page
strictEvaluation = [\t -> do
	writeTopTitle t "正格評価と非正格評価"
	text t "", \t -> do
	text t "* 正格評価では関数の引数を先に評価する", \t -> do
	text t "例:"
	preLine t
	itext t 1 "f x = x * 2"
	itext t 1 "f (3 + 8)"
	itext t 1 "-> f (11)"
	itext t 1 "-> 11 * 2"
	itext t 1 "-> 22", \t -> do
	text t "* 非正格評価では引数の評価の前に関数適用を行う", \t -> do
	text t "例:"
	preLine t
	itext t 1 "f (3 + 8)"
	itext t 1 "-> (3 + 8) * 2"
	itext t 1 "-> 11 * 2"
	itext t 1 "-> 22"
 ]

spaceLeak :: Page
spaceLeak = [\t -> do
	writeTopTitle t "非正格評価の特徴"
	text t "", \t -> do
	text t "* 正格評価よりも広い範囲の式を評価できる", \t -> do
	itext t 1 "- 正格評価では簡約が終了しない式も簡約可能", \t -> do
	text t "* 不要な引数の評価をしないので", \t -> do
	itext t 1 "- 使わない引数がある場合に有利", \t -> do
	text t "* 評価前の式を持ち続けるので", \t -> do
	itext t 1 "- 空間効率が低下する傾向がある"
 ]

useFoldl :: Page
useFoldl = [\t -> do
	writeTopTitle t "総和を求める"
	text t "", \t -> do
	text t "* リスト内の整数の総和を求める", \t -> do
	itext t 0 "sum :: Integer -> [Integer] -> Integer"
	itext t 0 "sum s [] = s"
	itext t 0 "sum s (n : ns) = sum (s + n) ns", \t -> do
	itext t 1 "sum 0 [1, 2, 3]"
	itext t 1 "-> sum (0 + 1) [2, 3]"
	itext t 1 "-> sum ((0 + 1) + 2) [3]"
	itext t 1 "-> sum (((0 + 1) + 2) + 3) []"
	itext t 1 "-> ((0 + 1) + 2) + 3"
	itext t 1 "-> (1 + 2) + 3"
	itext t 1 "-> 3 + 3"
	itext t 1 "-> 6"
 ]

useFoldl2 :: Page
useFoldl2 = [\t -> do
	writeTopTitle t "総和を求める"
	text t "", \t -> do
	text t "* 簡約の経過を見てみると", \t -> do
	itext t 1 "- 右向きの山のような形になっている", \t -> do
	itext t 1 "- メモリの使用量が一度ふくらみ、それから縮小", \t -> do
	text t "* プロファイラでメモリの使用量を見てみる", \t -> do
	itext t 1 "% ghc -with-rtsopts=\"-K512m\""
	itext t 2 "-prof -fprof-auto sum.hs", \t -> do
	itext t 1 "% ./sum +RTS -h -RTS", \t -> do
	itext t 1 "% hp2ps -c sum.hp", \t -> do
	itext t 1 "% ps2pdf sum.ps", \t -> do
	itext t 1 "% firefox sum.pdf"
 ]

useFoldl3 :: Page
useFoldl3 = [\t -> do
	writeTopTitle t "総和を求める"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/sum-out.png"), \t -> do
	text t "* 最大値が250MBくらいの山", \t -> do
	text t "* サンクの生長とともにメモリを確保していき", \t -> do
	text t "* 足し算によって値が決まるにしたがいメモリを解放"
 ]

useFoldl4 :: Page
useFoldl4 = [\t -> do
	writeTopTitle t "総和を求める"
	text t "", \t -> do
	text t "* sumは本来ならば定数空間で実行できる", \t -> do
	text t "* 今の実装ではO(n)空間を必要とする", \t -> do
	arrowIText t 1 "スペースリークが生じている", \t -> do
	text t "* 問題は最後まで関数適用するまで足し算が行われない点", \t -> do
	text t "* 関数適用よりも前に足し算を評価することを強制したい", \t -> do
	itext t 1 "- 正格評価を強制するということ", \t -> do
	text t "* Haskellには正格評価を強制する関数が用意されている", \t -> do
	itext t 1 "seq :: a -> b -> b", \t -> do
	itext t 1 "($!) :: (a -> b) -> a -> b"
 ]

aboutSeq :: Page
aboutSeq = [\t -> do
	writeTopTitle t "正格評価のための関数"
	text t "", \t -> do
	text t "* x `seq` yとすると", \t -> do
	itext t 1 "- yが評価される前にxの評価が強制される", \t -> do
	itext t 1 "- xの評価は弱頭部正規形(WHNF)まで", \t -> do
	itext t 2 "「弱頭部正規形」は上級編で", \t -> do
	itext t 2 "ここでは「リストの中身は評価しない」程度で", \t -> do
	text t "* ($!)は関数適用の正格評価版", \t -> do
	text t "* f $! xとすると", \t -> do
	itext t 1 "- 関数適用の前にxをWHNFまでに評価する", \t -> do
	itext t 1 "- f $! x = x `seq` f xということ"
 ]

useSeq :: Page
useSeq = [\t -> do
	writeTopTitle t "総和を効率的に求める"
	text t "", \t -> do
	text t "* 正格評価版のsumを書いてみる", \t -> do
	itext t 0 "sum :: Integer -> [Integer] -> Integer"
	itext t 0 "sum s [] = s"
	itext t 0 "sum s (n : ns) = let r = s + n in r `seq` sum r ns", \t -> do
	itext t 1 "sum 0 [1, 2, 3]"
	itext t 1 "-> (0 + 1) `seq` sum (0 + 1) [2, 3]"
	itext t 1 "-> sum 1 [2, 3]"
	itext t 1 "-> (1 + 2) `seq` sum (1 + 2) [3]"
	itext t 1 "-> sum 3 [3]"
	itext t 1 "-> (3 + 3) `seq` sum (3 + 3) []"
	itext t 1 "-> sum 6 []"
	itext t 1 "-> 6"
 ]

useSeq2 :: Page
useSeq2 = [\t -> do
	writeTopTitle t "総和を効率的に求める"
	text t "", \t -> do
	text t "* 常に一定の空間しか使っていないことに注意", \t -> do
	text t "* プロファイラでメモリ使用量を見てみよう", \t -> do
	itext t 1 "% ghc -prof -fprof-auto sum.hs"
	itext t 1 "% ./sum +RTS -h -RTS"
	itext t 1 "% hp2ps -c sum.hp"
	itext t 1 "% ps2pdf sum.ps"
	itext t 1 "% firefox sum.pdf"
 ]

useSeq3 :: Page
useSeq3 = [\t -> do
	writeTopTitle t "総和を効率的に求める"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/sum_strict.png"), \t -> do
	text t "* 35KBの一定量のメモリが使われている"
 ]

aboutFoldl' :: Page
aboutFoldl' = [\t -> do
	writeTopTitle t "foldl'"
	text t "", \t -> do
	text t "* 左たたみこみの場合、正格評価してほしい場合がほとんど", \t -> do
	text t "* 正格評価版のfoldlが用意されている", \t -> do
	itext t 1 "foldl' :: (a -> b -> a) -> a -> [b] -> a", \t -> do
	text t "* これを使うとsumは以下のように書ける", \t -> do
	itext t 1 "sum :: [Integer] -> Integer"
	itext t 1 "sum = foldl' (+) 0", \t -> do
	text t "* 反復的プロセスを使う場合", \t -> do
	itext t 1 "- foldl'を使うことを考えるべき"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 総和を求める計算を例とした", \t -> do
	text t "* 非正格(遅延)評価だと空間効率が大きく低下する", \t -> do
	text t "* 正格(先行)評価だと空間効率が良い", \t -> do
	text t "* 評価順を決める関数がある", \t -> do
	itext t 1 "seq :: a -> b -> b", \t -> do
	itext t 1 "($!) :: (a -> b) -> a -> b", \t -> do
	text t "* 性能低下の原因がスペースリークである場合", \t -> do
	itext t 1 "- seqや($!)によって適切な場所を正格評価にしてやる"
 ]
