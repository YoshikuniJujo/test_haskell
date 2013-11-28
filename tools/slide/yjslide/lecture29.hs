import Lecture

subtitle :: String
subtitle = "第29回 型構築子と正格性"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	mkSum, mkSum2, mkSum3, mkSum4, mkSum5, mkSum6,
	explanation, summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 型構築子によって値を作るとき", \t -> do
	text t "* 引数の値を評価しておきたい場合がある", \t -> do
	text t "* 遅延性の必要ないところでは正格評価のほうが効率が良い", \t -> do
	text t "* 型構築子のフィールドに!をつける"
 ]

mkSum :: Page
mkSum = [\t -> do
	writeTopTitle t "総和の例"
	text t "", \t -> do
	text t "* 良い例が見つからないので、以前の例を使い回す", \t -> do
	text t "* 和を保存する型というものを「わざわざ」作ってみる", \t -> do
	itext t 1 "% cat notStrictData.hs"
	itext t 1 "data Sum = Sum Integer deriving Show"
	itext t 1 ""
	itext t 1 "sum :: Sum -> [Integer] -> Sum"
	itext t 1 "sum s [] = s"
	itext t 1 "sum (Sum s) (x : xs) = sum (Sum $ s + x) xs"
	itext t 1 ""
	itext t 1 "main = print $ sum (Sum 0) [0 .. 10000000]"
 ]

mkSum2 :: Page
mkSum2 = [\t -> do
	writeTopTitle t "総和の例"
	text t "", \t -> do
	text t "* プロファイリング", \t -> do
	itext t 1 "% ghc -with-rtsopts=\"-K512m\" -prof"
	itext t 2 "-fprof-auto notStrictData.hs"
	itext t 1 "% ./notStrictData +RTS -h -RTS"
	itext t 1 "% hp2ps -c notStrictData.hp"
	itext t 1 "% ps2pdf notStrictData.ps"
	itext t 1 "% firefox notStrictData.pdf"
 ]

mkSum3 :: Page
mkSum3 = [\t -> do
	writeTopTitle t "総和の例"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/notStrictData.png"), \t -> do
	text t "* メモリを大量に確保し、解放するというパターン"
 ]

mkSum4 :: Page
mkSum4 = [\t -> do
	writeTopTitle t "総和の例"
	text t "", \t -> do
	text t "* 正格性フラグをつけてみる", \t -> do
	itext t 1 "% cat strictData.hs"
	itext t 1 "data Sum = Sum !Integer deriving Show"
	itext t 1 ""
	itext t 1 "sum :: Sum -> [Integer] -> Sum"
	itext t 1 "sum s [] = s"
	itext t 1 "sum (Sum s) (x : xs) = sum (Sum $ s + x) xs"
	itext t 1 ""
	itext t 1 "main = print $ sum (Sum 0) [0 .. 10000000]"
 ]

mkSum5 :: Page
mkSum5 = [\t -> do
	writeTopTitle t "総和の例"
	text t "", \t -> do
	text t "* プロファイリング", \t -> do
	itext t 1 "% ghc -with-rtsopts=\"-K512m\" -prof"
	itext t 2 "-fprof-auto strictData.hs"
	itext t 1 "% ./strictData +RTS -h -RTS"
	itext t 1 "% hp2ps -c strictData.hp"
	itext t 1 "% ps2pdf strictData.ps"
	itext t 1 "% firefox strictData.pdf"
 ]

mkSum6 :: Page
mkSum6 = [\t -> do
	writeTopTitle t "総和の例"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/strictData.png"), \t -> do
	text t "* メモリの使用量は一定"

 ]

explanation :: Page
explanation = [\t -> do
	writeTopTitle t "正格性フラグの意味"
	text t "", \t -> do
	text t "* 関数fを正格評価される関数にする", \t -> do
	itext t 1 "- f x = x `seq` ...のようにすれば良い", \t -> do
	itext t 1 "- BangPatternsを使えば以下のようにも書ける", \t -> do
	itext t 2 "f !x = ...", \t -> do
	text t "* データ構築子Cを正格評価したい", \t -> do
	itext t 1 "- データ構築子の実体は定義しないのでseqは使えない", \t -> do
	itext t 1 "- 特別な構文を用意する必要がある", \t -> do
	itext t 2 "C !Int"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* データ構築子は関数と同じようなもの", \t -> do
	text t "* 関数ならばseqを使って正格評価とすることが可能", \t -> do
	text t "* データ構築子はそれができないので", \t -> do
	text t "* 特別な構文が用意されている", \t -> do
	text t "* それが正格性フラグであり以下のように書く", \t -> do
	itext t 1 "data Foo = Foo !Int !Double !String"
 ]
