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
	examStateIO6
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

preludeMonadsTf :: Page
preludeMonadsTf = [\t -> do
	text t "* そういったことを行うためのパッケージが用意されている", \t -> do
	itext t 1 "- パッケージは「第28回 cabalの使いかた」で", \t -> do
	itext t 1 "- パッケージの名前はmonads-tf"
 ]
