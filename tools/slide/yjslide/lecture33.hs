import Lecture

subtitle :: String
subtitle = "第33回 リストとしての文字列"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, whatsList, iterateByList,
	essenceOfList, essenceOfList2,
	lookTwice, lookTwice2, lookTwice3, lookTwice4, lookTwice5, lookTwice6,
	randomAccess, randomAccess2, randomAccess3, randomAccess4, randomAccess5,
	randomAccess6, randomAccess7,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellでは文字列は文字の遅延リストとして表現される", \t -> do
	itext t 1 "type String = [Char]", \t -> do
	text t "* リストの持つ性質により用途によって向き不向きがある", \t -> do
	text t "* リストの扱いかたをまちがえると著しい性能低下が生じる", \t -> do
	text t "* 使いどころをまちがえなければ非常に有用", \t -> do
	text t "* 使い道によっては後でやる「配列としての文字列」を使う", \t -> do
	text t "* リストとしての文字列の", \t -> do
	itext t 1 "- 使いどころ", \t -> do
	itext t 1 "- 使いかた", \t -> do
	itext t 1 "- 注意点", \t -> do
	text t "* について学んでいこう"
 ]

whatsList :: Page
whatsList = [\t -> do
	writeTopTitle t "リストとは何か"
	text t "", \t -> do
	text t "* Haskellのリストは遅延リストである", \t -> do
	text t "* 遅延リストの典型的な動作は", \t -> do
	itext t 1 "- 要素はアクセスされるまで存在せず", \t -> do
	itext t 1 "- それ以上使われない要素はそのたびに捨てられる", \t -> do
	itext t 1 "- その性質を強調した別名は「ストリーム」", \t -> do
	text t "* 遅延リストの本質は何か?", \t -> do
	itext t 1 "- 「くりかえし」の実体化"
 ]

iterateByList :: Page
iterateByList = [\t -> do
	writeTopTitle t "リストによる「くりかえし」"
	text t "", \t -> do
	text t "* nからmまでの数を表示する", \t -> do
	itext t 1 "printNtoM :: Int -> Int -> IO ()"
	itext t 1 "printNtoM n m = mapM_ print [n .. m]", \t -> do
	text t "* nからmまでの数を奇数をとばして表示する", \t -> do
	itext t 1 "printEvenNtoM n m = mapM_ print $"
	itext t 2 "filter even [n .. m]", \t -> do
	text t "* 定義は「リストの値を作ってから表示」", \t -> do
	text t "* 動作は「値を作りながら表示」", \t -> do
	itext t 1 "- 「くりかえし」を実現している"
 ]

essenceOfList :: Page
essenceOfList = [\t -> do
	writeTopTitle t "リストの本質"
	text t "", \t -> do
	text t "* 遅延リストの本質とは何か?", \t -> do
	itext t 1 "- 「くりかえし」という「動作」の「実体化」", \t -> do
	text t "* 内側に「くりかえし」を保持している", \t -> do
	text t "* 「くりかえし」を行うために温められた構造", \t -> do
	text t "* リストをあとでつかおうと保存するのは", \t -> do
	itext t 1 "- 温めたピザをあとで食べようと取っておくこと", \t -> do
	text t "* リストの得意とするのは", \t -> do
	itext t 1 "- 作られるたびに消費されるような動作", \t -> do
	itext t 1 "- 皿回しのようなもの"
 ]

essenceOfList2 :: Page
essenceOfList2 = [\t -> do
	writeTopTitle t "リストの本質"
	text t "", \t -> do
	text t "* リストは「動作」を「実体化」したものなので", \t -> do
	itext t 1 "- 幽霊のように扱おう", \t -> do
	itext t 1 "- 見るまでは存在せず、見るとすぐに消える", \t -> do
	itext t 1 "- 各要素の生成からGCを短くすませるのがこつ", \t -> do
	text t "* 小さなリストは実体として扱っても良い", \t -> do
	text t "* 巨大なリストは幽霊だと考える必要がある", \t -> do
	itext t 1 "- 2回見てはならない", \t -> do
	itext t 1 "- 足は見ないで頭を見る"
 ]

lookTwice :: Page
lookTwice = [\t -> do
	writeTopTitle t "2度見禁止"
	text t "", \t -> do
	text t "* 巨大なリストを2回見るとどうなるか", \t -> do
	text t "* 大きなファイルを作っておく", \t -> do
	itext t 1 "% cat mkBigFile.hs"
	itext t 1 "main :: IO ()"
	itext t 1 "main = writeFile \"big.txt\" $ concat $"
	itext t 2 "replicate (10 ^ 6) \"1234567890\"", \t -> do
	itext t 1 "% runghc mkBigFile.hs", \t -> do
	itext t 1 "% ls -lh big.txt"
	itext t 1 "... 9.6M ..."
 ]

lookTwice2 :: Page
lookTwice2 = [\t -> do
	writeTopTitle t "2度見禁止"
	text t "", \t -> do
	text t "* 1度しか見ない場合", \t -> do
	itext t 1 "% cat lookOnce.hs"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "cnt <- readFile \"big.txt\""
	itext t 2 "putStrLn cnt", \t -> do
	itext t 1 "% ghc -prof -fprof-auto lookOnce.hs"
	itext t 1 "% ./lookOnce +RTS -h -RTS"
	itext t 1 "% hp2ps -c lookOnce.hp"
	itext t 1 "% ps2pdf lookOnce.ps"
	itext t 1 "% firefox lookOnce.pdf"
 ]

lookTwice3 :: Page
lookTwice3 = [\t -> do
	writeTopTitle t "2度見禁止"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/lookOnce.png"), \t -> do
	text t "* 9.6MBのファイルを読み込んでいるが", \t -> do
	text t "* 60kBから80kBのあいだでほぼ一定のメモリ使用量"
 ]

lookTwice4 :: Page
lookTwice4 = [\t -> do
	writeTopTitle t "2度見禁止", \t -> do
	text t "* 2度見る場合", \t -> do
	itext t 1 "% cat lookTwice.hs"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "cnt <- readFile \"big.txt\""
	itext t 2 "putStrLn cnt"
	itext t 2 "putStrLn cnt", \t -> do
	itext t 1 "% ghc -prof -fprof-auto lookTwice.hs"
	itext t 1 "% ./lookTwice +RTS -h -RTS"
	itext t 1 "% hp2ps -c lookTwice.hp"
	itext t 1 "% ps2pdf lookTwice.ps"
	itext t 1 "% firefox lookTwice.pdf"
 ]

lookTwice5 :: Page
lookTwice5 = [\t -> do
	writeTopTitle t "2度見禁止"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/profiling/new/lookTwice.png"), \t -> do
	text t "* リストの生成とともに120MBまでメモリの使用量が増加", \t -> do
	text t "* 2回目の表示とともにGCが行われメモリを解放", \t -> do
	text t "* 9.6MBのファイルを読み込むのに120MBのメモリを使用"
 ]

lookTwice6 :: Page
lookTwice6 = [\t -> do
	writeTopTitle t "2度見禁止"
	text t "", \t -> do
	text t "* 巨大なリストは生成とともに使われていけば", \t -> do
	itext t 1 "- ほぼ一定のメモリ使用量で実行できる", \t -> do
	text t "* リストを2回参照してしまうと", \t -> do
	itext t 1 "- 後で見るために保存される", \t -> do
	itext t 1 "- 空間効率の著しい低下", \t -> do
	text t "* 9.6MBのファイルを読み込むのに120MBのメモリが必要", \t -> do
	itext t 1 "- 12倍程度の領域が使われている", \t -> do
	itext t 1 "- CharはUTF-32なので4B使われる", \t -> do
	itext t 1 "- リストなので次のリストへのアドレスが必要", \t -> do
	itext t 1 "- Charはボックス化されている"
 ]

randomAccess :: Page
randomAccess = [\t -> do
	writeTopTitle t "ランダムアクセス禁止"
	text t "", \t -> do
	text t "* ランダムアクセスとは", \t -> do
	itext t 1 "- リストを頭から順に見ていくのではなく", \t -> do
	itext t 1 "- インデックスでいろいろなところを見ること", \t -> do
	text t "* リストのランダムアクセスはO(n)時間かかる", \t -> do
	text t "* 頭から順に要素を見ていく動作を比較対象として", \t -> do
	text t "* ランダム値によるランダムアクセスの効率を見る"
 ]

randomAccess2 :: Page
randomAccess2 = [\t -> do
	writeTopTitle t "ランダムアクセス禁止"
	text t "", \t -> do
	text t "* 両方で共通に使うIOのくりかえし関数を定義しておく", \t -> do
	itext t 1 "timesDo :: Int -> a -> (a -> IO a) -> IO ()"
	itext t 1 "timesDo 0 _ _ = return ()"
	itext t 1 "timesDo n x0 io = do"
	itext t 2 "x1 <- io x0"
	itext t 2 "timesDo (n - 1) x1 io", \t -> do
	itext t 1 "- IOの結果を次のIOの実行に使えるようにした"
 ]

randomAccess3 :: Page
randomAccess3 = [\t -> do
	writeTopTitle t "ランダムアクセス禁止"
	text t "", \t -> do
	text t "* 両方で共通に使う遅延しないreadFile関数を定義しておく", \t -> do
	itext t 0 "readFile' :: FilePath -> IO String"
	itext t 0 "readFile' fp = do"
	itext t 1 "cnt <- readFile fp"
	itext t 1 "putStr $ take (length cnt - length cnt) \"dummy\""
	itext t 1 "return cnt", \t -> do
	text t "- lengthを使用することによってStringの評価を強制"
 ]

randomAccess4 :: Page
randomAccess4 = [\t -> do
	writeTopTitle t "ランダムアクセス禁止", \t -> do
	text t "* シーケンシャルアクセスの場合", \t -> do
	itext t 1 "sequentialElem :: [a] -> Int -> IO (a, [a])"
	itext t 1 "sequentialElem (x : xs) len = do"
	itext t 2 "i <- randomRIO (0, len - 1)"
	itext t 2 "putStr $ show i ++ \":\""
	itext t 2 "return (x, xs)"
	itext t 1 "main = do"
	itext t 2 "cnt <- readFile' \"big.txt\""
	itext t 2 "timesDo 1000 cnt $ \\lst -> do"
	itext t 3 "(c, cs) <- sequentialElem lst (10 ^ 7)"
	itext t 3 "putChar c"
	itext t 3 "putChar '\\n'"
	itext t 3 "return cs"
 ]

randomAccess5 :: Page
randomAccess5 = [\t -> do
	writeTopTitle t "ランダムアクセス禁止"
	text t "", \t -> do
	text t "* ランダムアクセスとの比較用にできるだけ動作を同じに", \t -> do
	text t "* 本質的にしていることはファイルの頭から1000文字を表示", \t -> do
	text t "* プロファイルを取る", \t -> do
	itext t 1 "% ghc -prof -fprof-auto sequentialAccess.hs"
	itext t 1 "% ./sequentialAccess +RTS -p -RTS", \t -> do
	text t "* 0.47秒のうちsequentialElemが1.1%の時間", \t -> do
	text t "* ランダム値の表示を含めても0.005秒しかかからない"
 ]

randomAccess6 :: Page
randomAccess6 = [\t -> do
	writeTopTitle t "ランダムアクセス禁止", \t -> do
	text t "* ランダムアクセスの場合", \t -> do
	itext t 1 "randomElem :: [a] -> Int -> IO a"
	itext t 1 "randomElem xs len = do"
	itext t 2 "i <- randomRIO (0, len - 1)"
	itext t 2 "putStr $ show i ++ \":\""
	itext t 2 "return $ xs !! i"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "cnt <- readFile' \"big.txt\""
	itext t 2 "timesDo 1000 undefined $ \\_ -> do"
	itext t 3 "c <- randomElem cnt (10 ^ 7)"
	itext t 3 "putChar c"
	itext t 3 "putChar '\\n'"
 ]

randomAccess7 :: Page
randomAccess7 = [\t -> do
	writeTopTitle t "ランダムアクセス禁止"
	text t "", \t -> do
	text t "* リストからランダムに値を取り出している", \t -> do
	text t "* 24.96秒のうち97.2%がrandomElem", \t -> do
	text t "* ランダム値の表示も含めて24.26秒かかっている", \t -> do
	text t "* シーケンシャルアクセスの0.005秒と比較すると", \t -> do
	itext t 1 "- 5000倍程度の時間がかかっている"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* Haskellのデフォルトの文字列は文字のリスト", \t -> do
	text t "* 1度だけ頭から順にアクセスしていくのならこれを使う", \t -> do
	itext t 1 "- 定数空間でのアクセスが可能", \t -> do
	text t "* 巨大なリストは2回見てはいけない", \t -> do
	itext t 1 "- 空間効率が著しく低下する", \t -> do
	text t "* ランダムアクセスを避ける", \t -> do
	itext t 1 "- O(n)時間かかってしまう", \t -> do
	text t "* 単純なフィルタのようなプログラムにはStringを使う", \t -> do
	text t "* 保存やランダムアクセスが必要な場合", \t -> do
	itext t 1 "- 配列としての文字列の使用を考える"
 ]
