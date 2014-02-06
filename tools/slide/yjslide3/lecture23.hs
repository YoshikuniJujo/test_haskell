import Lecture

subtitle :: String
subtitle = "第23回 まとめ:オセロ(ウィンドウの処理)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, beginModule,
	importWX, importWX2, importModules,
	setConstants, setConstants2, setConstants3, setConstants4,
	setConstants5,
	makeWindow, makeWindow2, makeWindow3, makeWindow4, makeWindow5
--	aboutPaintLines
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ゲームの表現とその操作を作りAIも作ったので", \t -> do
	text t "* ウィンドウシステムとのやりとりを作ればオセロが完成する", \t -> do
	text t "* 描画関数と入力処理関数とを作る", \t -> do
	text t "* AIの動作はタイマー制御とする", \t -> do
	text t "* ユーザーの入力でゲームが開始し", \t -> do
	text t "* 次がどちらのターンかチェックし", \t -> do
	itext t 1 "- ユーザーなら入力を有効にしタイマーを無効にする", \t -> do
	itext t 1 "- AIなら入力を無効にしタイマーを有効にする"
 ]

beginModule :: Page
beginModule = [\t -> do
	writeTopTitle t "Windowモジュール"
	text t "", \t -> do
	text t "* Window.hsを作り以下を書き込もう", \t -> do
	itext t 1 "module Window(", \t -> do
	itext t 1 ") where"
	itext t 1 "", \t -> do
	itext t 1 "import Game (",  \t -> do
	itext t 2 "Game, Turn(..), X(..), Y(..), Disk(..),", \t -> do
	itext t 2 "turn, disks, initGame, nextGame)", \t -> do
	itext t 1 "import AI (aiN)", \t -> do
	text t "* 読み込んでおく", \t -> do
	itext t 1 "% ghci Window.hs", \t -> do
	itext t 1 "*Window>"
 ]

importWX :: Page
importWX = [\t -> do
	writeTopTitle t "wxHaskell"
	text t "", \t -> do
	text t "* クロスプラットフォームなツールキットとして", \t -> do
	itext t 1 "- wxWidgetsというライブラリがある", \t -> do
	text t "* それのHaskellへのバインディングがwxHaskell", \t -> do
	itext t 1 "- 状態変化と並行実行を前提としている", \t -> do
	arrowIText t 1 "あまり関数型的でない", \t -> do
	text t "* もっと関数型的に書けるライブラリもある", \t -> do
	text t "* しかし理論が難しくなるので今回はwxHaskellを使う"
 ]

importWX2 :: Page
importWX2 = [\t -> do
	writeTopTitle t "wxHaskell"
	text t "", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t 0 "import Graphics.UI.WX (", \t -> do
	itext t 1 "start, Prop(..), set, on,", \t -> do
	itext t 1 "frameFixed, close, text, minsize, sz,", \t -> do
	itext t 1 "layout, widget,", \t -> do
	itext t 1 "Timer, timer, interval, command, enabled,", \t -> do
	itext t 1 "Panel, panel, paint, click, charKey, repaint,", \t -> do
	itext t 1 "DC, Rect, BrushKind(..),", \t -> do
	itext t 1 "brushKind, brushColor, black, white,", \t -> do
	itext t 1 "Point, Point2(..), line, circle, drawText,", \t -> do
	itext t 1 "Var, varCreate, varGet, varUpdate)", \t -> do
	itext t 2 "(1分)"
 ]

importModules :: Page
importModules = [\t -> do
	writeTopTitle t "その他のモジュール"
	text t "", \t -> do
	text t "* 以下の関数も使うので先にimportしておく", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t 1 "import Control.Arrow ((***))", \t -> do
	itext t 1 "import Control.Monad (forM_)", \t -> do
	itext t 1 "import Data.Maybe (fromMaybe)", \t -> do
	itext t 1 "import Data.List (partition)"
 ]

setConstants :: Page
setConstants = [\t -> do
	writeTopTitle t "定数の定義"
	text t "", \t -> do
	text t "* コード中で使う定数を定義しておこう", \t -> do
	text t "* まずはAIに関する定数", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t 1 "aiForesee :: Int", \t -> do
	itext t 1 "aiForesee = 3"
	itext t 1 "", \t -> do
	itext t 1 "aiWaitMs :: Int", \t -> do
	itext t 1 "aiWaitMs = 1000", \t -> do
	text t "* aiForeseeはAIが先読みする手の数", \t -> do
	text t "* aiWaitMsはAIの待ち時間(単位はミリ秒)"
 ]

setConstants2 :: Page
setConstants2 = [\t -> do
	writeTopTitle t "定数の定義"
	text t "", \t -> do
	text t "* 盤の大きさや配置に関する定数", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t 1 "leftMargin, rightMargin, topMargin, bottomMargin,", \t -> do
	itext t 2 "squareSize, discRadius :: Int", \t -> do
	itext t 1 "leftMargin = 10", \t -> do
	itext t 1 "rightMargin = 50", \t -> do
	itext t 1 "topMargin = 10", \t -> do
	itext t 1 "bottomMargin = 20", \t -> do
	itext t 1 "squareSize = 30", \t -> do
	itext t 1 "discRadius = 12", \t -> do
	text t "* ...Marginは盤の外周のスペースの大きさ", \t -> do
	text t "* squareSizeはマスの大きさでdiscRadiusは石の半径"
 ]

setConstants3 :: Page
setConstants3 = [\t -> do
	writeTopTitle t "定数の定義"
	text t "", \t -> do
	text t "* メッセージ表示に関する定数", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t 1 "msgLeft, charSpace :: Int", \t -> do
	itext t 1 "msgLeft = 80", \t -> do
	itext t 1 "charSpace = 20", \t -> do
	text t "* msgLeftはメッセージ表示の左のスペース", \t -> do
	text t "* charSpaceは次の文字の占める空間の大きさ"
 ]

setConstants4 :: Page
setConstants4 = [\t -> do
	writeTopTitle t "定数の定義"
	text t "", \t -> do
	text t "* 既出の定数から計算できる定数も定義しておく", \t -> do
	text t "* まずは、盤の右と下の縁とウィンドウのサイズ", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t 1 "boundRight, boundBottom,", \t -> do
	itext t 2 "windowWidth, windowHeight :: Int", \t -> do
	itext t 1 "boundRight = leftMargin + squareSize * 8", \t -> do
	itext t 1 "boundBottom = topMargin + squareSize * 8", \t -> do
	itext t 1 "windowWidth = boundRight + rightMargin", \t -> do
	itext t 1 "windowHeight =", \t -> do
	itext t 2 "boundBottom + bottomMargin + charSpace * 3"
 ]

setConstants5 :: Page
setConstants5 = [\t -> do
	writeTopTitle t "定数の定義"
	text t "", \t -> do
	text t "* メッセージの表示に関する定数", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t 1 "msgTop, msgTop2, msgLeft2 :: Int", \t -> do
	itext t 1 "msgTop = boundBottom + bottomMargin", \t -> do
	itext t 1 "msgTop2 = msgTop + charSpace", \t -> do
	itext t 1 "msgLeft2 = msgLeft + charSpace", \t -> do
	text t "* msgTopはメッセージの1行目", \t -> do
	text t "* msgTop2はメッセージの2行目", \t -> do
	text t "* msgLeft2はメッセージの左から2番目の位置", \t -> do
	text t "* 次はウィンドウを出してみよう"
 ]

makeWindow :: Page
makeWindow = [\t -> do
	writeTopTitle t "ウィンドウを出す", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t (-1) "othello :: IO ()", \t -> do
	itext t (-1) "othello = do", \t -> do
	itext t 0 "vg <- varCreate initGame", \t -> do
	itext t 0 "f <- frameFixed [text := \"othello\"]", \t -> do
	itext t 0 "t <- timer f [interval := aiWaitMs, enabled := False]", \t -> do
	itext t 0 "p <- panel f [on (charKey 'q') := close f,"
	itext t 1 "on paint := paintGame vg]", \t -> do
	itext t 0 "set t [on command := aiPlace vg p t]", \t -> do
	itext t 0 "set p [on click := userPlace vg p t]", \t -> do
	itext t 0 "set f [layout :=", \t -> do
	itext t 1 "minsize (sz windowWidth windowHeight) $ widget p ]"
 ]

makeWindow2 :: Page
makeWindow2 = [\t -> do
	writeTopTitle t "othello関数の説明"
	text t "", \t -> do
	text t "* ここにothello関数の説明を書く", \t -> do
	text t "* とくに[name := value]の形について"
 ]

makeWindow3 :: Page
makeWindow3 = [\t -> do
	writeTopTitle t "スタブ"
	text t "", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t 0 "paintGame :: Var Game -> DC a -> Rect -> IO ()", \t -> do
	itext t 0 "paintGame _ _ _ = return ()"
	itext t 0 "", \t -> do
	itext t 0 "aiPlace :: Var Game -> Panel () -> Timer -> IO ()", \t -> do
	itext t 0 "aiPlace _ _ _ = return ()"
	itext t 0 "", \t -> do
	itext t 0 "userPlace ::", \t -> do
	itext t 1 "Var Game -> Panel () -> Timer -> Point -> IO ()", \t -> do
	itext t 0 "userPlace _ _ _ _ = return ()"
 ]

makeWindow4 :: Page
makeWindow4 = [\t -> do
	writeTopTitle t "エクスポートリスト"
	text t "", \t -> do
	text t "* Windowモジュールが公開する必要のある関数はstart, othelloなので", \t -> do
	text t "* Window.hsのモジュール宣言を以下のように書き換える", \t -> do
	itext t 1 "module Window (start, othello) where", \t -> do
	text t "* Mainモジュールを作る", \t -> do
	text t "* othello.hsを作り以下を書き込もう", \t -> do
	itext t 1 "module Main (main) where"
	itext t 1 "", \t -> do
	itext t 1 "import Window (start, othello)"
	itext t 1 "", \t -> do
	itext t 1 "main :: IO ()", \t -> do
	itext t 1 "main = start othello"
 ]

makeWindow5 :: Page
makeWindow5 = [\t -> do
	writeTopTitle t ""
	text t "", \t -> do
	text t "* コンパイルして実行してみよう", \t -> do
	itext t 1 "% ghc othello.hs", \t -> do
	itext t 1 "./othello", \t -> do
	text t "* ウィンドウが表示されたはずだ", \t -> do
	text t "* 'q'キーで終了しよう"
 ]

aboutPaintLines :: Page
aboutPaintLines = [\t -> do
	writeTopTitle t "paintLines"
	text t "", \t -> do
	text t "* まずは盤のマスを区切る網を書く関数を作ろう", \t -> do
	text t "* wxHaskell上では表示関数は(DC a)という値を受け取る", \t -> do
	text t "* これは描画の際に必要となる", \t -> do
	itext t 1 "- DCはdevice contextの略", \t -> do
	text t ""
 ]
