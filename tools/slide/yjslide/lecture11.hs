import Lecture

subtitle :: String
subtitle = "第11回 ファイル入出力"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
	useInteract, useInteract2, useInteract3, useInteract4, useInteract5,
	useInteractSummary,
	useGetContents, useGetContents2, useGetContents3, useGetContents4,
	useGetContents5,
	usePutStr, badReadWrite, badReadWrite2,
	useGetLine, useOpenFile, useOpenFile2, useHIsEOF,
	charCode, newLine, buffering, binaryMode, useSeek, useWait,
	basic, basic2, basic3,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ファイル入出力について学ぶ", \t -> do
	text t "* いくつかのパラダイムが混在している", \t -> do
	text t "* そのため意外な落とし穴がある", \t -> do
	text t "* より先進的な機構については第53回「iteratee」で"
 ]

useInteract :: Page
useInteract = [\t -> do
	writeTopTitle t "フィルタを作る"
	text t "", \t -> do
	text t "* Unix系のOSではフィルタという考えかたがある", \t -> do
	itext t 1 "- 標準入力から入力を受け取り", \t -> do
	itext t 1 "- 何らかの加工を行い", \t -> do
	itext t 1 "- 標準出力に書き出す", \t -> do
	text t "* 標準入力はデフォルトではキーボードからのインプット", \t -> do
	text t "* 標準出力はデフォルトではモニタへの出力", \t -> do
	text t "* 標準入出力はファイルにつなぎかえることができる", \t -> do
	text t "* 標準入出力は他のコマンドにつなぎかえることができる"
 ]

useInteract2 :: Page
useInteract2 = [\t -> do
	writeTopTitle t "フィルタを作る"
	text t "", \t -> do
	text t "* フィルタは(String -> String)型の関数とみなせる", \t -> do
	arrowIText t 1 "関数型言語との相性が良い", \t -> do
	text t "* (String -> String)型の関数からフィルタを作る関数", \t -> do
	itext t 1 "interact :: (String -> String) -> IO ()", \t -> do
	text t "* Unix系OSで簡単なフィルタを作るのならばこれが使える"
 ]

useInteract3 :: Page
useInteract3 = [\t -> do
	writeTopTitle t "フィルタを作る"
	text t "", \t -> do
	text t "* 最も単純な例", \t -> do
	itext t 1 "% cat id.hs"
	itext t 2 "main :: IO ()"
	itext t 2 "main = interact id", \t -> do
	itext t 1 "% runghc id.hs"
	itext t 1 "hello"
	itext t 1 "hello"
	itext t 1 "world"
	itext t 1 "world", \t -> do
	text t "* 標準入力からの入力をそのまま標準出力に書き出している", \t -> do
	text t "* 入力と出力が交互に行われていることに注目"
 ]

useInteract4 :: Page
useInteract4 = [\t -> do
	writeTopTitle t "フィルタを作る"
	text t "", \t -> do
	text t "* 入力値をreverseして出力", \t -> do
	itext t 1 "% cat rev.hs"
	itext t 2 "main :: IO ()"
	itext t 2 "main = interact reverse", \t -> do
	itext t 1 "% runghc rev.hs"
	itext t 1 "hello"
	itext t 1 "world", \t -> do
	itext t 1 "[Ctrl-D]"
	itext t 1 "dlrow"
	itext t 1 "olleh%", \t -> do
	text t "* Ctrl-Dで入力を終了するまで出力されない"
 ]

useInteract5 :: Page
useInteract5 = [\t -> do
	writeTopTitle t "フィルタを作る"
	text t "", \t -> do
	text t "* 出力に十分なだけの入力を受けとった時点で出力", \t -> do
	arrowIText t 1 "Haskellの遅延評価という性質を十二分に利用", \t -> do
	text t "* 最終的に(String -> String)という形を作れば良い", \t -> do
	arrowIText t 1 "問題解決に集中できる", \t -> do
	text t "* ほとんどのコードを純粋な関数に保つことができる", \t -> do
	arrowIText t 1 "IOに関する余計な複雑さを排除できる", \t -> do
	text t "* リダイレクトやパイプを使うことで柔軟な使いかたが", \t -> do
	dvArrowShort t
	text t "フィルタで十分な問題に対しては最高のスタイル"
 ]

useInteractSummary :: Page
useInteractSummary = [\t -> do
	writeTopTitle t "フィルタを作る(まとめ)"
	text t "", \t -> do
	text t "* 標準入力から標準出力へのフィルタという考えかた", \t -> do
	text t "* Unixで使う部品を作るときの理想的なスタイル", \t -> do
	text t "* コードの純粋さを保ちたいときの最適解", \t -> do
	text t "* 日常のプログラミングではフィルタで十分なことは多い", \t -> do
	itext t 1 "- ファイルを読み書きするにはリダイレクトを使う", \t -> do
	text t "* IOモナドというパラダイムより手前にあるやりかた", \t -> do
	text t "* 積極的に使おう"
 ]

useGetContents :: Page
useGetContents = [\t -> do
	writeTopTitle t "遅延リストを使う"
	text t "", \t -> do
	text t "* interactでは標準入力からの入力しか使えない", \t -> do
	text t "* 複数の入力を扱いたい", \t -> do
	text t "* それぞれの入力に対してひとつのリストを割り当てる", \t -> do
	text t "* リストの要素が必要とされたときに入力値が読み込まれる", \t -> do
	text t "* コード側から見たイメージはひとつの大きなリストがある", \t -> do
	text t "* 動作としてのイメージは読み込みたびに大きくなるリスト", \t -> do
	text t "* リストの前のほうの要素は2回以上使われなければGCされる", \t -> do
	arrowIText t 1 "定数空間でファイルの読み込みが可能"
 ]

useGetContents2 :: Page
useGetContents2 = [\t -> do
	writeTopTitle t "遅延リストを使う", \t -> do
	text t "* このパラダイムを採用している関数に以下のものがある", \t -> do
	itext t 1 "readFile :: FilePath -> IO String", \t -> do
	itext t 1 "getContents :: IO String", \t -> do
	itext t 1 "hGetContents :: Handle -> IO String", \t -> do
	text t "* getContentsは標準入力とリストを結びつける", \t -> do
	itext t 1 "- Stringは[Char]の別名であることに注意", \t -> do
	text t "* 以下でinputという遅延リストと標準入力が結びつけられる", \t -> do
	itext t 1 "input <- getContents", \t -> do
	text t "* interactはgetContentsを使って以下のように書ける", \t -> do
	itext t 1 "interact f = do"
	itext t 2 "input <- getContents"
	itext t 2 "putStr (f input)"
 ]

useGetContents3 :: Page
useGetContents3 = [\t -> do
	writeTopTitle t "遅延リストを使う"
	text t "", \t -> do
	text t "* ファイルを読み込む", \t -> do
	itext t 1 "readFile :: FilePath -> IO String", \t -> do
	text t "* FilePathはStringの別名なのでファイル名は文字列で指定", \t -> do
	text t "* 以下でリストcntとファイル\"some.txt\"とが結びつく", \t -> do
	itext t 1 "cnt <- readFile \"some.txt\"", \t -> do
	text t "* cntの要素が必要となるたびにファイルから読み込む", \t -> do
	itext t 1 "- unsafeInteraleaveIOという関数が使われている", \t -> do
	itext t 1 "- この関数については後のほうの講義で扱う"
 ]

useGetContents4 :: Page
useGetContents4 = [\t -> do
	writeTopTitle t "遅延リストを使う"
	text t "", \t -> do
	text t "* readFileとgetContentsはhGetContentsで定義されている", \t -> do
	itext t 1 "getContents = hGetContents stdin", \t -> do
	itext t 1 "readFile name ="
	itext t 2 "openFile name ReadMode >>= hGetContents", \t -> do
	text t "* stdinは最初から用意されているHandle", \t -> do
	text t "* openFileはファイルを指定してHandleを返す関数", \t -> do
	text t "* Handleについては後でやる"
 ]

useGetContents5 :: Page
useGetContents5 = [\t -> do
	writeTopTitle t "遅延リストを使う"
	text t "", \t -> do
	text t "* ファイルを読み込んで表示する関数", \t -> do
	itext t 1 "putTestFile :: IO ()"
	itext t 1 "putTestFile = do"
	itext t 2 "cnt <- readFile \"test.txt\""
	itext t 2 "putStr cnt", \t -> do
	text t "* もちろん以下のように書いても良い", \t -> do
	itext t 1 "putTestFile = readFile \"test.txt\" >>= putStr"
 ]

usePutStr :: Page
usePutStr = [\t -> do
	writeTopTitle t "出力"
	text t "", \t -> do
	text t "* 文字列を出力する関数には以下のものがある", \t -> do
	itext t 1 "writeFile :: FilePath -> String -> IO ()", \t -> do
	itext t 1 "appendFile :: FilePath -> String -> IO ()", \t -> do
	itext t 1 "putStr :: String -> IO ()", \t -> do
	itext t 1 "hPutStr :: Handle -> String -> IO ()", \t -> do
	text t "* writeFileはファイルへの書き出し", \t -> do
	text t "* appendFileはファイルへの追加書き込み", \t -> do
	text t "* putStrは標準出力への書き出し", \t -> do
	text t "* より基本的な関数としてhPutStrがある"
 ]

badReadWrite :: Page
badReadWrite = [\t -> do
	writeTopTitle t "落とし穴"
	text t "", \t -> do
	text t "* readFileによって得られるのは遅延リストである", \t -> do
	text t "* readFileはリストの最後の要素が評価されると終了する", \t -> do
	text t "* 以下の例を見てみよう", \t -> do
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "cnt <- readFile \"tmp.txt\""
	itext t 2 "writeFile \"tmp.txt\" cnt", \t -> do
	text t "* これを実行すると以下のエラーが生じる", \t -> do
	itext t 1 "some.hs: tmp.txt:"
	itext t 2 "openFile: resource busy (file is locked)", \t -> do
	text t "* tmp.txtが使用中なので書き込み用に開けなかった"
 ]

badReadWrite2 :: Page
badReadWrite2 = [\t -> do
	writeTopTitle t "落とし穴"
	text t "", \t -> do
	text t "* 遅延リストに関係づけられたIO処理はIO全体に散らばる", \t -> do
	text t "* プログラムが複雑になると予期せぬエラーを生じ得る", \t -> do
	text t "* 読み込みエラーがコードの広い範囲に生じ得るため", \t -> do
	itext t 1 "- 例外を捕捉するのも難しくなる", \t -> do
	dvArrowShort t
	text t "readFileやgetContentsはIOの部分が単純なコードにのみ", \t -> do
	itext t 1 "- フィルタ的な用途にしぼる", \t -> do
	itext t 1 "- 走査は一回のみとする"
 ]

useGetLine :: Page
useGetLine = [\t -> do
	writeTopTitle t "行単位の読み込み"
	text t "", \t -> do
	text t "* 遅延リストとIOの絡み合いによる問題を回避したい", \t -> do
	text t "* そのためには行単位の読み込みが使える", \t -> do
	text t "* readFile等よりもよりIOモナドのパラダイムに忠実", \t -> do
	itext t 1 "getLine :: IO String", \t -> do
	itext t 1 "hGetLine :: Handle -> IO String", \t -> do
	text t "* 使ってみる", \t -> do
	itext t 1 "> getLine"
	itext t 1 "hello"
	itext t 1 "\"hello\"", \t -> do
	text t "* ファイルからの読み込みにはファイルを開きHandleを入手"
 ]

useOpenFile :: Page
useOpenFile = [\t -> do
	writeTopTitle t "ファイルを開く"
	text t "", \t -> do
	text t "* ファイル入出力にはHandleを使う", \t -> do
	text t "* ファイルを開いてHandleを入手する関数が用意されている", \t -> do
	itext t 1 "openFile :: FilePath -> IOMode -> IO Handle", \t -> do
	text t "* ファイルを閉じる関数", \t -> do
	itext t 1 "hClose :: Handle -> IO ()", \t -> do
	text t "* 使い終わったHandleを確実に閉じてくれる関数", \t -> do
	itext t 1 "withFile :: FilePath -> IOMode ->"
	itext t 3 "(Handle -> IO r) -> IO r", \t -> do
	text t "* withFileだけ覚えておけば良い"
 ]

useOpenFile2 :: Page
useOpenFile2 = [\t -> do
	writeTopTitle t "ファイルを開く"
	text t "", \t -> do
	text t "* IOModeとは何か", \t -> do
	itext t 1 "data IOMode"
	itext t 2 "= ReadMode"
	itext t 2 "| WriteMode"
	itext t 2 "| AppendMode"
	itext t 2 "| ReadWriteMode", \t -> do
	text t "* 読んで字のごとし"
 ]

useHIsEOF :: Page
useHIsEOF = [\t -> do
	writeTopTitle t "ファイルの内容を表示する"
	text t "", \t -> do
	text t "* ファイルの終了をチェックする関数", \t -> do
	itext t 1 "hIsEOF :: Handle -> IO Bool", \t -> do
	text t "* 文字列を表示して改行を出力する関数", \t -> do
	itext t 1 "putStrLn :: String -> IO ()", \t -> do
	text t "* ファイルの中身を表示する関数を作る", \t -> do
	itext t 1 "printFile :: FilePath -> IO ()"
	itext t 1 "printFile fp = withFile fp ReadMode $ \\h -> do"
	itext t 2 "eof <- hIsEOF h"
	itext t 2 "if eof"
	itext t 3 "then return ()"
	itext t 3 "else hGetLine h >>= putStrLn"
 ]

charCode :: Page
charCode = [\t -> do
	writeTopTitle t "文字コード"
	text t "", \t -> do
	text t "* 文字(列)の読み込み、書き出し関数は文字コードを考慮", \t -> do
	text t "* Haskellの文字はユニコードで保存されているので", \t -> do
	itext t 1 "- 読み込み時にはユニコードへ変換", \t -> do
	itext t 1 "- 書き出し時にはユニコードから変換", \t -> do
	text t "* システムのデフォルトのエンコードを調べる", \t -> do
	itext t 1 "> localeEncoding"
	itext t 1 "UTF-8", \t -> do
	text t "* Handleごとにエンコードを設定できる", \t -> do
	itext t 1 "hSetEncoding :: Handle -> TextEncoding -> IO ()", \t -> do
	text t "* Handleごとのエンコードを調べる", \t -> do
	itext t 1 "hGetEncoding :: Handle -> IO (Maybe TextEncoding)"
 ]

newLine :: Page
newLine = [\t -> do
	writeTopTitle t "改行文字"
	text t "", \t -> do
	text t "* 改行文字はウィンドウズとユニックス系OSとで異なる", \t -> do
	text t "* Haskellは基本的に改行を'\\n'で表現する", \t -> do
	text t "* よってウィンドウズでは", \t -> do
	itext t 1 "- 読み込み時には'\\r\\n' -> '\\n'の変換を", \t -> do
	itext t 1 "- 書き出し時には'\\n' -> '\\r\\n'の変換を行う", \t -> do
	text t "* 改行の変換を設定する関数がある", \t -> do
	itext t 1 "hSetNewLineMode :: Handle -> NewlineMode -> IO ()", \t -> do
	text t "* 変換を行わない場合", \t -> do
	itext t 1 "hSetNewLineMode h noNewlineTranslation"
 ]

buffering :: Page
buffering = [\t -> do
	writeTopTitle t "バッファリング"
	text t "", \t -> do
	text t "* バッファリングを設定することができる", \t -> do
	itext t 1 "hSetBuffering :: Handle -> BufferMode -> IO ()", \t -> do
	text t "* バッファリングの設定を調べる", \t -> do
	itext t 1 "hGetBuffering :: Handle -> IO BufferMode", \t -> do
	text t "* BufferModeには何があるか?", \t -> do
	itext t 1 "NoBuffering", \t -> do
	itext t 1 "LineBuffering", \t -> do
	itext t 1 "BlockBuffering (Maybe Int)", \t -> do
	text t "* BlockBufferingの引数は", \t -> do
	itext t 1 "- Just nのときはn個のアイテムをバッファリングし", \t -> do
	itext t 1 "- Nothingのときは実装依存となる"
 ]

binaryMode :: Page
binaryMode = [\t -> do
	writeTopTitle t "バイナリモード"
	text t "", \t -> do
	text t "* バイナリモードを設定する関数", \t -> do
	itext t 1 "hSetBinaryMode :: Handle -> Bool -> IO ()", \t -> 
	itext t 1 "- hSetBinaryMode h Trueとするのは以下と同じ", \t -> do
	itext t 2 "hSetEncoding h char8"
	itext t 2 "hSetNewlineMode h noNewlineTranslation", \t -> do
	text t "* バイナリモードでファイルを開く関数が用意されている", \t -> do
	itext t 1 "openBinaryFile :: FilePath -> IOMode -> IO Handle", \t -> do
	itext t 1 "withBinaryFile :: FilePath -> IOMode ->"
	itext t 4 "(Handle -> IO r) -> IO r"
 ]

useSeek :: Page
useSeek = [\t -> do
	writeTopTitle t "シーク"
	text t "", \t -> do
	text t "* ファイル中を自由に移動できる", \t -> do
	itext t 1 "hSeek :: Handle -> SeekMode -> Integer -> IO ()", \t -> do
	text t "* SeekModeは以下のどれか", \t -> do
	itext t 1 "AbsoluteSeek", \t -> do
	itext t 1 "RelativeSeek", \t -> do
	itext t 1 "SeekFromEnd", \t -> do
	text t "* 読んで字のごとし"
 ]

useWait :: Page
useWait = [\t -> do
	writeTopTitle t "入力値が使用可能かチェック"
	text t "", \t -> do
	text t "* チェックのときに指定された時間だけ待つ関数", \t -> do
	itext t 1 "hWaitForInput :: Handle -> Int -> IO Bool", \t -> do
	itext t 1 "- 指定の時間内に入力があればただちにTrueを返す", \t -> do
	itext t 1 "- 指定の時間内に入力がなければFalseを返す", \t -> do
	itext t 1 "- 負の時間が指定されれば入力が来るまで待つ", \t -> do
	itext t 1 "- 時間はミリ秒で指定する", \t -> do
	text t "* その時点での使用可能な入力値の有無を返す関数", \t -> do
	itext t 1 "hReady :: Handle -> IO Bool"
 ]

basic :: Page
basic = [\t -> do
	writeTopTitle t "より低レベルな入出力"
	text t "", \t -> do
	text t "* よりプリミティブな入出力関数がある", \t -> do
	text t "* ポインタで指定された番地から指定のバイト数を出力する", \t -> do
	itext t 1 "hPutBuf :: Handle -> Ptr a -> Int -> IO ()", \t -> do
	text t "* メモリのアロケーションとメモリへの書き込みが必要", \t -> do
	itext t 0 "putBytes :: Handle -> [Word8] -> IO ()"
	itext t 0 "putBytes h ws = allocaBytes (length ws) $ \\p -> do"
	itext t 1 "pokeArray p ws"
	itext t 1 "hPutBuf h p (length ws)"
 ]

basic2 :: Page
basic2 = [\t -> do
	writeTopTitle t "より低レベルな入出力"
	text t "", \t -> do
	text t "* ポインタで指定された番地へ指定のバイト数を入力する", \t -> do
	itext t 1 "hGetBuf :: Handle -> Ptr a -> Int -> IO Int", \t -> do
	itext t 1 "- 出力は読み込んだバイト数", \t -> do
	text t "* メモリのアロケーションとメモリからの読み出しが必要", \t -> do
	itext t 0 "getBytes :: Handle -> Int -> IO [Word8]"
	itext t 0 "getBytes h n = allocaBytes n $ \\p -> do"
	itext t 1 "s <- hGetBuf h p n"
	itext t 1 "peekArray s p"
 ]

basic3 :: Page
basic3 = [\t -> do
	writeTopTitle t "より低レベルな入出力"
	text t "", \t -> do
	text t "* 入力関数は入力待ちの種類によって3種類ある", \t -> do
	text t "* hGetBufは入力が指定されたバイト数に達するまで待つ", \t -> do
	text t "* hGetBufSomeは入力が1バイトでもあれば実行される", \t -> do
	text t "* hGetBufNonBlockingは入力が無くても直ちに実行される"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* interactで十分ならそれを使う", \t -> do
	text t "* readFileは簡単だが複雑なプログラムでは避けるべき", \t -> do
	text t "* テキストファイルならばhGetLineも選択肢になる", \t -> do
	text t "* 文字コードや改行文字の変換を制御する関数がある", \t -> do
	text t "* 文字コードと改行文字をバイナリモードにする関数がある", \t -> do
	text t "* ファイルのシークについて見た", \t -> do
	text t "* 入力値の存在をチェックする関数を見た", \t -> do
	text t "* 細かい制御のためのよりプリミティブな関数を学んだ"
 ]
