import Lecture

subtitle :: String
subtitle = "第23回 まとめ:オセロ(ウィンドウの処理)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, beginModule,
	importWX, importWX2, importModules,
	setConstants, setConstants2, setConstants3, setConstants4,
	setConstants5,
	makeWindow, makeWindow2, makeWindow3, makeWindow4, makeWindow5,
	makeWindow6, makeWindow7, makeWindow8, makeWindow9, makeWindow10,
	aboutStub,
	aboutPaintGame, aboutPaintGame2, aboutPaintGame3, aboutPaintGame4,
	aboutPaintGame5,
	aboutPaintLines
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
	text t "* 一行ずつ見ていこう", \t -> do
	itext t 1 "vg <- varCreate initGame", \t -> do
	text t "* initGameを初期値にして「変数」を作っている", \t -> do
	text t "* この変数はIOモナド内で値を変化させることができる"
 ]

makeWindow3 :: Page
makeWindow3 = [\t -> do
	writeTopTitle t "othello関数の説明"
	text t "", \t -> do
	text t "* まずはトップレベルのウィンドウを作る", \t -> do
	itext t 1 "f <- frameFixed [text := \"othello\"]", \t -> do
	text t "* サイズの変更できないウィンドウを作っている", \t -> do
	text t "* ウィンドウのタイトルを\"othello\"とした", \t -> do
	text t "* (:=)は値構築演算子", \t -> do
	itext t 1 "- ':'で始まる演算子は値構築子となる", \t -> do
	itext t 1 "- (text := \"othello\")はtextと\"othello\"を含む値", \t -> do
	itext t 1 "- textはウィンドウにタイトルを設定する関数を保持", \t -> do
	text t "* frameFixed関数のなかで", \t -> do
	itext t 1 "- \"othello\"を引数にしてtext内の関数が実行される"
 ]

makeWindow4 :: Page
makeWindow4 = [\t -> do
	writeTopTitle t "othello関数の説明"
	text t "", \t -> do
	text t "* タイマーと描画領域(パネル)を作る", \t -> do
	itext t 1 "t <- timer f ["
	itext t 2 "interval := aiWaitMs,"
	itext t 2 "enabled := False ]", \t -> do
	itext t 1 "p <- panel f ["
	itext t 2 "on (charKey 'q') := close f,"
	itext t 2 "on paint := paintGame vg ]", \t -> do
	text t "* タイマーを作りその間隔を設定し無効化", \t -> do
	text t "* パネルを作り'q'キーに終了を設定し描画関数を設定", \t -> do
	text t "* interval, enabled, on ...は値の設定のための関数を保持"
 ]

makeWindow5 :: Page
makeWindow5 = [\t -> do
	writeTopTitle t "othello関数の説明"
	text t "", \t -> do
	text t "* タイマーと描画領域の動作を設定", \t -> do
	itext t 1 "set t [on command := aiPlace vg p t]", \t -> do
	itext t 1 "set p [on click := userPlace vg p t]", \t -> do
	text t "* タイマー作動時の動作とパネルクリック時の動作を設定", \t -> do
	text t "* ターンが変化する際に自他を", \t -> do
	itext t 1 "無効化、有効化するために引数としてp, tを持つ"
 ]

makeWindow6 :: Page
makeWindow6 = [\t -> do
	writeTopTitle t "othello関数の説明"
	text t "", \t -> do
	text t "* パネルをウィンドウ上に配置する", \t -> do
	itext t 0 "set f [layout :="
	itext t 1 "minsize (sz windowWidth windowHeight) $ widget p ]", \t -> do
	text t "* 最小のサイズを指定してパネルをウィンドウ上に配置"
 ]

makeWindow7 :: Page
makeWindow7 = [\t -> do
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

makeWindow8 :: Page
makeWindow8 = [\t -> do
	writeTopTitle t "Mainモジュール"
	text t "", \t -> do
	text t "* Windowモジュールが公開する関数はstart, othello", \t -> do
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

makeWindow9 :: Page
makeWindow9 = [\t -> do
	writeTopTitle t "start"
	text t "", \t -> do
	text t "* main関数を再掲する", \t -> do
	itext t 1 "main = start othello", \t -> do
	text t "* start関数はothello関数に", \t -> do
	itext t 1 "- 初期化を追加し", \t -> do
	itext t 1 "- メインループを追加している"
 ]

makeWindow10 :: Page
makeWindow10 = [\t -> do
	writeTopTitle t "コンパイル、実行"
	text t "", \t -> do
	text t "* コンパイルして実行してみよう", \t -> do
	itext t 1 "% ghc othello.hs", \t -> do
	itext t 1 "./othello", \t -> do
	text t "* ウィンドウが表示されたはずだ", \t -> do
	text t "* 'q'キーで終了しよう"
 ]

aboutStub :: Page
aboutStub = [\t -> do
	writeTopTitle t "スタブを埋める"
	text t "", \t -> do
	text t "* あとはスタブを埋めていけばオセロが完成する", \t -> do
	text t "* 現在あるスタブは以下の通り", \t -> do
	itext t 1 "- paintGame: 盤とメッセージを描画する", \t -> do
	itext t 1 "- aiPlace: AIが石を置く処理", \t -> do
	itext t 1 "- userPlace: ユーザーが石を置く処理"
 ]

aboutPaintGame :: Page
aboutPaintGame = [\t -> do
	writeTopTitle t "paintGame"
	text t "", \t -> do
	text t "* まずはpaintGameで使う以下の関数のスタブを作ろう", \t -> do
	itext t 1 "- paintLines: 盤の網を描画する", \t -> do
	itext t 1 "- drawDisk: 位置と色を指定して石を描画する", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t 1 "paintLines :: DC a -> IO ()", \t -> do
	itext t 1 "paintLines _ = return ()"
	itext t 1 "", \t -> do
	itext t 1 "drawDisk :: DC a -> (X, Y) -> Disk -> IO ()", \t -> do
	itext t 1 "drawDisk _ _ _ = return ()", \t -> do
	text t "* DCはdevice contextの略で描画に必要な情報を渡している"
 ]

aboutPaintGame2 :: Page
aboutPaintGame2 = [\t -> do
	writeTopTitle t "paintGame"
	text t "", \t -> do
	text t "* paintGameは盤と石以外に以下のメッセージを表示する", \t -> do
	itext t 1 "- 白と黒の石の数", \t -> do
	itext t 1 "- どちらのターンか", \t -> do
	text t "* これらは直接paintGame関数内に書き込んでしまおう", \t -> do
	text t "* 以下をWindow.hsに書き込もう", \t -> do
	itext t 0 "paintGame :: Var Game -> DC a -> Rect -> IO ()", \t -> do
	itext t 1 "(続く)"
 ]

aboutPaintGame3 :: Page
aboutPaintGame3 = [\t -> do
	writeTopTitle t "paintGame", \t -> do
	itext t (- 1.3) "paintGame vg dc _ = do", \t -> do
	itext t (- 0.8) "g <- varGet vg", \t -> do
	itext t (- 0.8) "let (b, w) = length *** length $", \t -> do
	itext t (- 0.3) "partition ((== Black) . snd) $ disks g", \t -> do
	itext t (- 0.8) "paintLines dc", \t -> do
	itext t (- 0.8) "forM_ (disks g) $ uncurry $ drawDisk dc", \t -> do
	itext t (- 0.8) "case turn g of", \t -> do
	itext t (- 0.3) "Turn Black -> drawText dc \"*\" (Point msgLeft msgTop) []", \t -> do
	itext t (- 0.3) "Turn White -> drawText dc \"*\" (Point msgLeft msgTop2) []", \t -> do
	itext t (- 0.3) "_ -> return ()", \t -> do
	itext t (- 0.8) "drawText dc (\"Black: \" ++ show b) (Point msgLeft2 msgTop) []", \t -> do
	itext t (- 0.8) "drawText dc (\"White: \" ++ show w) (Point msgLeft2 msgTop2) []"
 ]

aboutPaintGame4 :: Page
aboutPaintGame4 = [\t -> do
	writeTopTitle t "paintGame"
	text t "", \t -> do
	text t "* varGetで現位のゲームの状態を入手する", \t -> do
	text t "* let (b, w) = ...では黒白それぞれの石の数を入手する", \t -> do
	itext t 1 "- (***)はそれぞれの関数をタプルの要素に適用", \t -> do
	itext t 1 "- partitionは条件を満たすものとそれ以外とを分ける", \t -> do
	text t "* paintLinesで盤の網を描画し", \t -> do
	text t "* forM_ (disks g) $ ...で盤上のすべての石について", \t -> do
	text t "* uncurry $ drawDisk dcは(石の位置, 色)から石を描画する", \t -> do
	text t "* case turn g ofで今のターンによって場合分けし", \t -> do
	itext t 1 "適切な場所に\"*\"を表示する", \t -> do
	text t "* そのあとの2つのdrawTextは黒白それぞれの石の数を表示"
 ]

aboutPaintGame5 :: Page
aboutPaintGame5 = [\t -> do
	writeTopTitle t "paintGame"
	text t "", \t -> do
	text t "* コンパイル、実行", \t -> do
	itext t 1 "% ghc othello.hs", \t -> do
	itext t 1 "./othello", \t -> do
	text t "* 画面の下のほうに以下のように表示される", \t -> do
	itext t 1 "* Black: 2"
	itext t 1 "  White: 2", \t -> do
	text t "* 'q'キーを終して終了しよう"
 ]

aboutPaintLines :: Page
aboutPaintLines = [\t -> do
	writeTopTitle t "paintLines", \t -> do
	text t "* 盤のマスを区切る網を書く関数を作ろう", \t -> do
	text t "* Window.hsのスタブを以下で置き換えよう", \t -> do
	itext t 0 "paintLines dc =", \t -> do
	itext t 1 "mapM_ lineV [0 .. 8] >> mapM_ lineH [0 .. 8]", \t -> do
	itext t 1 "where", \t -> do
	itext t 1 "cx x = leftMargin + x * squareSize", \t -> do
	itext t 1 "cy y = topMargin + y * squareSize", \t -> do
	itext t 1 "lineV x = line dc", \t -> do
	itext t 2 "(Point (cx x) topMargin)", \t -> do
	itext t 2 "(Point (cx x) boundBottom) []", \t -> do
	itext t 1 "lineH y = line dc", \t -> do
	itext t 2 "(Point leftMargin (cy y))", \t -> do
	itext t 2 "(Point boundRight (cy y)) []"
 ]
