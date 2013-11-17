module Main where

import Lecture

subtitle :: String
subtitle = "第17回 モナド変換子"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
	examErrorState, examErrorState2, examErrorState3, examErrorState4,
	examErrorState5, examErrorState6, examErrorStateSummary,
	examStateIO, examStateIO2, examStateIO3, examStateIO4, examStateIO5,
	examStateIO6, examStateIOSummary,
	stateMaybeStateIO,
	stateT, stateT2, stateTSummary,
	maybeState, maybeState2, maybeState3, maybeState4, maybeState5,
	maybeStateSummary,
	maybeIO, maybeIO2, maybeIO3, maybeIO4, maybeIOSummary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 「第15回 いろいろなモナド」でいろいろなモナドを見た", \t -> do
	text t "* それらのモナドを組み合わせて使いたくなることがある", \t -> do
	itext t 1 "- 失敗する可能性のある状態を取る計算", \t -> do
	itext t 1 "- 環境を持ちログを記録する計算", \t -> do
	itext t 1 "- などなど", \t -> do
	text t "* IOモナドと組み合わせて使いたくなることもある"
 ]

examErrorState :: Page
examErrorState = [\t -> do
	writeTopTitle t "失敗と状態のある計算"
	text t "", \t -> do
	text t "* 失敗の可能性のある計算は以下のようになる", \t -> do
	itext t 1 "data Maybe a = Just a | Nothing"
	itext t 1 "return = Just"
	itext t 1 "Just x >>= f = f x"
	itext t 1 "Nothing >>= f = Nothing", \t -> do
	text t "* 状態を取る計算は以下のようになる", \t -> do
	itext t 1 "newtype State s a = State {"
	itext t 2 "runState :: s -> (a, s) }"
	itext t 1 "return a = State $ \\s -> (a, s)"
	itext t 1 "(State x) >>= f = State $ \\s ->"
	itext t 2 "let (v, s') = x s in runState (f v) s'"
 ]

examErrorState2 :: Page
examErrorState2 = [\t -> do
	writeTopTitle t "失敗と状態のある計算"
	text t "", \t -> do
	text t "* それぞれの型の定義をよく見る", \t -> do
	itext t 1 "data Maybe a = Just a | Nothing"
	itext t 1 "newtype State s a = State {"
	itext t 2 "runState :: s -> (a, s) }", \t -> do
	text t "* この2つを合わせた型を作ってみよう", \t -> do
	itext t 1 "newtype StateMaybe = StateMaybe {"
	itext t 2 "runStateMaybe :: s -> Maybe (a, s) }"
 ]

examErrorState3 :: Page
examErrorState3 = [\t -> do
	writeTopTitle t "失敗と状態のある計算"
	text t "", \t -> do
	text t "* モナドにする", \t -> do
	itext t 1 "newtype StateMaybe = StateMaybe {"
	itext t 2 "runStateMaybe :: s -> Maybe (a, s) }", \t -> do
	itext t 1 "return a = StateMaybe $ \\s -> Just (a, s)"
	itext t 1 "StateMaybe x >>= f = StateMaybe $ \\s ->"
	itext t 2 "case x s of"
	itext t 3 "Just (v, s') -> runStateMaybe (f v) s'"
	itext t 3 "_ -> Nothing"
 ]

examErrorState4 :: Page
examErrorState4 = [\t -> do
	writeTopTitle t "失敗と状態のある計算", \t -> do
	text t "* 基本的な関数を定義する", \t -> do
	itext t 1 "put :: s -> StateMaybe s ()"
	itext t 1 "put s = StateMaybe $ \\_ -> Just ((), s)"
	itext t 1 "", \t -> do
	itext t 1 "get :: StateMaybe s s"
	itext t 1 "get = StateMaybe $ \\s -> Just (s, s)"
	itext t 1 "", \t -> do
	itext t 1 "modify :: (s -> s) -> StateMaybe s ()"
	itext t 1 "modify f = get >>= put . f"
	itext t 1 "", \t -> do
	itext t 1 "nothing :: StateMaybe s a"
	itext t 1 "nothing = StateMaybe $ \\_ -> Nothing"

 ]

examErrorState5 :: Page
examErrorState5 = [\t -> do
	writeTopTitle t "失敗と状態のある計算"
	text t "", \t -> do
	text t "* 使用例", \t -> do
	itext t 1 "addMemory :: Int -> StateMaybe Int ()"
	itext t 1 "addMemory n = modify (+ n)"
	itext t 1 "", \t -> do
	itext t 1 "subMemory :: Int -> StateMaybe Int ()"
	itext t 1 "subMemory n = modify (subtract n) >> checkMemory"
	itext t 1 "", \t -> do
	itext t 1 "checkMemory :: StateMaybe Int ()"
	itext t 1 "checkMemory = do"
	itext t 2 "s <- get"
	itext t 2 "when (s < 0) nothing"
 ]

examErrorState6 :: Page
examErrorState6 = [\t -> do
	writeTopTitle t "失敗と状態のある計算"
	text t "", \t -> do
	text t "* 使用例", \t -> do
	itext t 1 "subAll :: Int -> [Int] -> StateMaybe Int Int"
	itext t 1 "subAll x ys = do"
	itext t 2 "addMemory n"
	itext t 2 "mapM_ subMemory ss"
	itext t 2 "get"
	text t "", \t -> do
	text t "* 状態としてメモリーを持った計算", \t -> do
	text t "* 引き算の結果が負になったらその時点でNothingを返す"
 ]

examErrorStateSummary :: Page
examErrorStateSummary = [\t -> do
	writeTopTitle t "失敗と状態のある計算(まとめ)"
	text t "", \t -> do
	text t "* 失敗する可能性のある状態を持つ計算を実装した", \t -> do
	text t "* MaybeモナドとStateモナドの両方の性質を持つ", \t -> do
	text t "* 型は以下のようになる", \t -> do
	itext t 1 "newtype StateMaybe s a = StateMaybe {"
	itext t 2 "runStateMaybe :: s -> Maybe (a, s) }", \t -> do
	text t "* 基本となる以下の関数を定義した", \t -> do
	itext t 1 "return, (>>=), put, get, modify, nothing"
 ]

examStateIO :: Page
examStateIO = [\t -> do
	writeTopTitle t "状態と入出力のある計算"
	text t "", \t -> do
	text t "* 状態と入出力の両方を持つ計算ができれば便利", \t -> do
	text t "* StateIO型を定義してみよう", \t -> do
	itext t 1 "newtype StateIO s a = StateIO {"
	itext t 2 "runStateIO :: s -> IO (a, s) }"
	itext t 1 "", \t -> do
	itext t 1 "instance Monad (StateIO s) where"
	itext t 2 "return x = StateIO $ \\s -> return (x, s)"
	itext t 2 "StateIO x >>= f = StateIO $ \\s -> do"
	itext t 3 "(v, s') <- x s"
	itext t 3 "runStateIO (f v) s'"
 ]

examStateIO2 :: Page
examStateIO2 = [\t -> do
	writeTopTitle t "状態と入出力のある計算"
	text t "", \t -> do
	text t "* 基本的な関数の定義", \t -> do
	itext t 1 "put :: s -> StateIO s ()"
	itext t 1 "put x = StateIO $ \\_ -> return ((), x)"
	itext t 1 "", \t -> do
	itext t 1 "get :: StateIO s s"
	itext t 1 "get = StateIO $ \\s -> return (s, s)"
	itext t 1 "", \t -> do
	itext t 1 "modify :: (s -> s) -> StateIO s ()"
	itext t 1 "modify f = get >>= put . f"
 ]

examStateIO3 :: Page
examStateIO3 = [\t -> do
	writeTopTitle t "状態と入出力のある計算"
	text t "", \t -> do
	text t "* 使用例", \t -> do
	itext t 1 "add :: Int -> StateIO Int ()"
	itext t 1 "add x = do"
	itext t 2 "modify (+ x)"
	itext t 2 "StateIO $ \\s -> do"
	itext t 3 "putStrLn $ \"add \" ++ show x"
	itext t 3 "return ((), s)"
 ]

examStateIO4 :: Page
examStateIO4 = [\t -> do
	writeTopTitle t "状態と入出力のある計算"
	text t "", \t -> do
	text t "* 使用例", \t -> do
	itext t 1 "test :: StateIO Int Int"
	itext t 1 "test = do"
	itext t 2 "add 8"
	itext t 2 "add 9"
	itext t 2 "s <- get"
	itext t 2 "add s"
	itext t 2 "get"
 ]

examStateIO5 :: Page
examStateIO5 = [\t -> do
	writeTopTitle t "状態と入出力のある計算"
	text t "", \t -> do
	text t "* StateIO内でのIOの実行", \t -> do
	itext t 1 "- addの例では以下のようになっている", \t -> do
	itext t 1 "StateIO $ \\s -> do"
	itext t 2 "putStrLn $ \"add \" ++ show x"
	itext t 2 "return ((), s)", \t -> do
	itext t 1 "- この構造を関数としてまとめてみる", \t -> do
	itext t 1 "lift :: IO a -> StateIO s a"
	itext t 1 "lift io = StateIO $ \\s -> do"
	itext t 2 "ret <- io"
	itext t 2 "return (ret, s)"
 ]

examStateIO6 :: Page
examStateIO6 = [\t -> do
	writeTopTitle t "状態と入出力のある計算"
	text t "", \t -> do
	text t "* liftを使ってaddを書き換える", \t -> do
	itext t 1 "add x = do"
	itext t 2 "modify (+ x)"
	itext t 2 "lift $ putStrLn $ \"add \" ++ show x"
 ]

examStateIOSummary :: Page
examStateIOSummary = [\t -> do
	writeTopTitle t "状態と入出力を持つ計算(まとめ)"
	text t "", \t -> do
	text t "* 状態と入出力を持つ計算を作った", \t -> do
	text t "* 型は以下の通り", \t -> do
	itext t 1 "newtype StateIO s a = StateIO {"
	itext t 2 "runStateIO :: s -> IO (a, s) }", \t -> do
	text t "* 基本的な関数を定義した", \t -> do
	itext t 1 "put, get, modify", \t -> do
	text t "* IOをStateIOに「持ち上げる」関数を定義した", \t -> do
	itext t 1 "lift :: IO a -> StateIO s a"
	itext t 1 "lift io = StateIO $ \\s -> do"
	itext t 2 "ret <- io"
	itext t 2 "return (ret, s)"
 ]

stateMaybeStateIO :: Page
stateMaybeStateIO = [\t -> do
	writeTopTitle t "StateMaybeとStateIO"
	text t "", \t -> do
	text t "* 型の比較", \t -> do
	itext t 1 "newtype StateMaybe s a = StateMaybe {"
	itext t 2 "runStateMaybe :: s -> Maybe (a, s) }", \t -> do
	itext t 1 "newtype StateIO s a = StateIO {"
	itext t 2 "runStateIO :: s -> IO (a, s) }", \t -> do
	text t "* これらを以下のようにまとめることができる", \t -> do
	itext t 1 "newtype StateT s m a = StateT {"
	itext t 2 "runStateT :: s -> m (a, s) }", \t -> do
	text t "* StateTは基盤となるモナドに状態を追加する"
 ]

stateT :: Page
stateT = [\t -> do
	writeTopTitle t "StateT", \t -> do
	text t "* 基本的な関数の定義", \t -> do
	itext t 1 "(>>=) :: StateT s m a -> (a -> StateT s m b) ->"
	itext t 2 "StateT s m b", \t -> do
	itext t 1 "StateT x >>= f = StateT $ \\s -> do"
	itext t 2 "(v, s') <- x s"
	itext t 2 "runStateT (f v) s'"
	itext t 1 "", \t -> do
	itext t 1 "put :: s -> StateT s m ()"
	itext t 1 "put x = StateT $ \\_ -> return ((), x)"
	itext t 1 "", \t -> do
	itext t 1 "get :: StateT s m s"
	itext t 1 "get = StateT $ \\s -> return (s, s)"
 ]

stateT2 :: Page
stateT2 = [\t -> do
	writeTopTitle t "StateT"
	text t "", \t -> do
	text t "* liftを定義する", \t -> do
	itext t 1 "lift :: m a -> StateIO s m a"
	itext t 1 "lift m = StateIO $ \\s -> do"
	itext t 2 "ret <- m"
	itext t 2 "return (ret, s)"
 ]

stateTSummary :: Page
stateTSummary = [\t -> do
	writeTopTitle t "StateT(まとめ)"
	text t "", \t -> do
	text t "* 他のモナドに状態を追加するモナド変換子を作った", \t -> do
	text t "* 型は以下のようになる", \t -> do
	itext t 1 "newtype StateT s m a = StateT {"
	itext t 2 "runStateT :: s -> m (a, s) }", \t -> do
	text t "* 基本的な関数を定義した", \t -> do
	itext t 1 "(>>=), put, get", \t -> do
	text t "* 持ち上げ関数liftをより一般的にした"
 ]

maybeState :: Page
maybeState = [\t -> do
	writeTopTitle t "失敗と状態のある計算"
	text t "", \t -> do
	text t "* 失敗と状態のある計算をさっきは以下のようにした", \t -> do
	itext t 1 "newtype StateMaybe s a = StateMaybe {"
	itext t 2 "runStateMaybe :: s -> Maybe (a, s) }", \t -> do
	text t "* 以下のようにすることも考えられる", \t -> do
	itext t 1 "newtype MaybeState s a = MaybeState {"
	itext t 2 "runMaybeState :: s -> (Maybe a, s) }", \t -> do
	text t "* Maybe (a, s)ではなく(Maybe a, s)とした"
 ]

maybeState2 :: Page
maybeState2 = [\t -> do
	writeTopTitle t "失敗と状態のある計算2"
	text t "", \t -> do
	text t "* モナド関数の定義", \t -> do
	itext t 1 "return :: a -> MaybeState s a"
	itext t 1 "return x = MaybeState $ \\s -> (Just x, s)"
	itext t 1 "", \t -> do
	itext t 1 "(>>=) :: MaybeState s a -> (a -> MaybeState s b)"
	itext t 2 "-> MaybeState s b"
	itext t 1 "MaybeState x >>= f = MaybeState $ \\s ->"
	itext t 2 "case x s of"
	itext t 3 "(Just v, s') -> runMaybeState (f v) s'"
	itext t 3 "(Nothing, s') -> (Nothing, s')"
 ]

maybeState3 :: Page
maybeState3 = [\t -> do
	writeTopTitle t "失敗と状態のある計算2"
	text t "", \t -> do
	text t "* 状態用の関数の定義", \t -> do
	itext t 1 "put :: s -> MaybeState s ()"
	itext t 1 "put x = MaybeState $ \\_ -> (Just (), x)", \t -> do
	itext t 1 "get :: MaybeState s s"
	itext t 1 "get = MaybeState $ \\s -> (Just s, s)"
	text t "", \t -> do
	text t "* エラー用の関数の定義", \t -> do
	itext t 1 "nothing :: MaybeState s a"
	itext t 1 "nothing = MaybeState $ \\s -> (Nothing, s)"
 ]

maybeState4 :: Page
maybeState4 = [\t -> do
	writeTopTitle t "失敗と状態のある計算2"
	text t "", \t -> do
	text t "* 使用例"
	itext t 1 "addMemory, subMemory :: Int -> MaybeState Int ()"
	itext t 1 "addMemory n = modify (+ n)", \t -> do
	itext t 1 "subMemory n = modify (subtract n) >> checkMemory"
	itext t 1 "", \t -> do
	itext t 1 "checkMemory :: MaybeState Int ()"
	itext t 1 "checkMemory = do"
	itext t 2 "s <- get"
	itext t 2 "when (s < 0) nothing"
 ]

maybeState5 :: Page
maybeState5 = [\t -> do
	writeTopTitle t "失敗と状態のある計算2"
	text t "", \t -> do
	text t "* 使用例", \t -> do
	itext t 1 "subAll :: Int -> [Int] -> MaybeState Int Int"
	itext t 1 "subAll n ss = do"
	itext t 2 "addMemory n"
	itext t 2 "mapM_ subMemory ss"
	itext t 2 "get"
 ]

maybeStateSummary :: Page
maybeStateSummary = [\t -> do
	writeTopTitle t "失敗と状態のある計算2(まとめ)"
	text t "", \t -> do
	text t "* 失敗と状態のある計算の作りかたのもうひとつの例", \t -> do
	itext t 1 "- Maybe (a, s)ではなく(Maybe a, s)とした", \t -> do
	text t "* 前の定義とは違い、エラー後も状態が渡され続ける", \t -> do
	itext t 1 "- 対話環境でのエラー等にはこっち"
 ]

maybeIO :: Page
maybeIO = [\t -> do
	writeTopTitle t "失敗と入出力のある計算"
	text t "", \t -> do
	text t "* 今度は失敗と入出力のある計算について考えてみる", \t -> do
	text t "* 例: ファイルの読み出し前にファイルの存在をチェック", \t -> do
	itext t 1 "- ファイルが存在すればその内容を返し", \t -> do
	itext t 1 "- 存在しなければ、その後の計算は行わない"
 ]

maybeIO2 :: Page
maybeIO2 = [\t -> do
	writeTopTitle t "失敗と入出力のある計算", \t -> do
	text t "* 型とモナド関数を定義する", \t -> do
	itext t 1 "newtype MaybeIO a = MaybeIO {"
	itext t 2 "runMaybeIO :: IO (Maybe a) }", \t -> do
	itext t 1 "return :: a -> MaybeIO a"
	itext t 1 "return x = MaybeIO $ return $ Just x", \t -> do
	itext t 1 "(>>=) :: MaybeIO a -> (a -> MaybeIO b) ->"
	itext t 2 "MaybeIO b"
	itext t 1 "MaybeIO io >>= f = do"
	itext t 2 "mx <- io"
	itext t 2 "case mx of"
	itext t 3 "Just x -> f x"
	itext t 3 "_ -> return Nothing"
 ]

maybeIO3 :: Page
maybeIO3 = [\t -> do
	writeTopTitle t "失敗と入出力のある計算"
	text t "", \t -> do
	text t "* 失敗用の関数を定義", \t -> do
	itext t 1 "nothing :: MaybeIO a"
	itext t 1 "nothing = MaybeIO $ return Nothing", \t -> do
	text t "* ファイルを読み込む関数", \t -> do
	itext t 1 "- ファイルが存在しなければNothingを返す", \t -> do
	itext t 1 "- 厳密には例外を補足する必要がある", \t -> do
	itext t 1 "maybeReadFile :: FilePath -> MaybeIO String"
	itext t 1 "maybeReadFile fp = do"
	itext t 2 "ex <- doesFileExist fp"
	itext t 2 "if ex"
	preLine t
	itext t 3 "then readFile fp"
	itext t 3 "else nothing"
 ]

maybeIO4 :: Page
maybeIO4 = [\t -> do
	writeTopTitle t "失敗と入出力のある計算"
	text t "", \t -> do
	text t "* 使用例", \t -> do
	itext t 1 "test :: MaybeIO ()"
	itext t 1 "test = do"
	itext t 2 "fp <- getLine"
	itext t 2 "cnt <- maybeReadFile fp"
	itext t 2 "putStr cnt"
	text t "", \t -> do
	text t "* 入力した名前のファイルを読み込む", \t -> do
	text t "* ファイルが存在しなければその後の計算は行われない"
 ]

maybeIOSummary :: Page
maybeIOSummary = [\t -> do
	writeTopTitle t "失敗と入出力のある計算"
	text t "", \t -> do
	text t "* 失敗と入出力のある計算のモナドを定義した", \t -> do
	text t "* 予想可能な例外が存在するIOを扱う場合", \t -> do
	itext t 1 "- 例外を補足しこの種のモナドにしたほうがクリーン", \t -> do
	itext t 1 "- 例外を本当に例外的な場面だけに", \t -> do
	text t "* 例外処理に関しては後の講義で行う"
 ]

preludeMonadsTf :: Page
preludeMonadsTf = [\t -> do
	text t "* そういったことを行うためのパッケージが用意されている", \t -> do
	itext t 1 "- パッケージは「第28回 cabalの使いかた」で", \t -> do
	itext t 1 "- パッケージの名前はmonads-tf"
 ]
