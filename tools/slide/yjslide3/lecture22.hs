import Control.Monad

import Lecture

subtitle :: String
subtitle = "第22回 まとめ:オセロ(AI)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	aboutTools, aboutTools2, aboutTools3, aboutTools4, aboutTools5,
	beginModule, aboutScore, aboutScore2, aboutScore3, aboutScore4,
	aboutScore5, scoreTable2, aboutTable, aboutTable2, aboutTable3,
	scoreTable1, aboutTable4,
	aboutEvaluate, aboutEvaluate2, aboutEvaluate3, aboutEvaluate4,
	aboutEvaluate5, aboutEvaluate6, aboutEvaluate7
	-- scoreTable2, scoreTable1
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* オセロの例を作ってきた", \t -> do
	text t "* 今回はAIの部分を作る", \t -> do
	text t "* マスごとにポイントをふる", \t -> do
	text t "* 何手か先を読み最善手を選ぶ", \t -> do
	text t "* 相手も同じアルゴリズムを採用すると仮定する", \t -> do
	text t "* それなりに強い、と思う", \t -> do
	text t "* 演者よりは強い", \t -> do
	text t "* 演者が対戦すると1勝3敗くらい"
 ]

aboutTools :: Page
aboutTools = [\t -> do
	writeTopTitle t "flipEnum"
	text t "", \t -> do
	text t "* まずはAIモジュールで使うより一般的な関数を作る", \t -> do
	itext t 1 "flipEnum, forMaybe, maximumBySnd", \t -> do
	text t "* flipEnumは順番のある値をひっくり返す", \t -> do
	text t "* data ABCD = A | B | C | D | Eを考えると", \t -> do
	itext t 1 "flipEnum A == E, flipEnum D == B ...", \t -> do
	text t "* Tools.hsに以下を追加しよう", \t -> do
	itext t 0 "flipEnum :: (Enum a, Bounded a) => a -> a", \t -> do
	itext t 0 "flipEnum x = toEnum $", \t -> do
	itext t 1 "fromEnum (maxBound `asTypeOf` x) - fromEnum x"
 ]

aboutTools2 :: Page
aboutTools2 = [\t -> do
	writeTopTitle t "forMaybe"
	text t "", \t -> do
	text t "* mapMaybeという関数がある", \t -> do
	itext t 1 "mapMaybe :: (a -> Maybe b) -> [a] -> [b]", \t -> do
	text t "* 関数を適用した結果がJustのものだけを集めるmap", \t -> do
	text t "* この関数の引数を入れ換えたものがあると便利なので", \t -> do
	text t "* Tools.hsに以下を書き込もう", \t -> do
	itext t 1 "forMaybe :: [a] -> (a -> Maybe b) -> [b]", \t -> do
	itext t 1 "forMaybe = flip mapMaybe", \t -> do
	text t "* mapMaybeはData.Maybeモジュールの関数なので", \t -> do
	text t "* Tools.hsのモジュール宣言の下に以下を書き込もう", \t -> do
	itext t 1 "import Data.Maybe (mapMaybe)", \t -> do
	text t "* Tools.hsのエクスポートリストにforMaybeを追加しよう"
 ]

aboutTools3 :: Page
aboutTools3 = [\t -> do
	writeTopTitle t "maximumBySnd"
	text t "", \t -> do
	text t "* タプルの第二要素で比較した最大値を返す関数", \t -> do
	text t "* maximumByとcompareとonを使うと簡単に作れる", \t -> do
	itext t 1 "maximumBy :: (a -> a -> Ordering) -> [a] -> a", \t -> do
	itext t 1 "compare :: Ord a => a -> a -> Ordering", \t -> do
	itext t 1 "on :: (b -> b -> c) -> (a -> b) -> a -> a -> c", \t -> do
	text t "* maximumByは第一引数の関数で比較した最大値を返す", \t -> do
	text t "* compareは2つの値を比較する", \t -> do
	text t "* onは第一引数の比較関数を実行する前に", \t -> do
	itext t 1 "それぞれの値に第二引数の関数を適用する"
 ]

aboutTools4 :: Page
aboutTools4 = [\t -> do
	writeTopTitle t "maximumBySnd"
	text t "", \t -> do
	text t "* maximumBySndは以下のようになるだろう", \t -> do
	itext t 1 "- 比較する値のそれぞれにまずはsndを適用して", \t -> do
	itext t 1 "- それらをcompareで比較するような関数を", \t -> do
	itext t 1 "- maximumByの第一引数とする", \t -> do
	text t "* 以下をTools.hsに書き込もう", \t -> do
	itext t 1 "maximumBySnd :: Ord b => [(a, b)] -> (a, b)", \t -> do
	itext t 1 "maximumBySnd = maximumBy $ on compare snd", \t -> do
	text t "* maximumBySndをエクスポートリストに追加しよう", \t -> do
	itext t 1 "module Tools ("
	itext t 2 "scc, prd, foldlMaybe, modifyList,"
	itext t 2 "flipEnum, forMaybe, maximumBySnd"
	itext t 1 ") where"
 ]

aboutTools5 :: Page
aboutTools5 = [\t -> do
	writeTopTitle t "maximumBySnd"
	text t "", \t -> do
	text t "* maximumByはData.Listの関数", \t -> do
	text t "* onはData.Functionの関数", \t -> do
	text t "* Tools.hsのモジュール宣言の下に以下を書き込もう", \t -> do
	itext t 1 "import Data.List (maximumBy)", \t -> do
	itext t 1 "import Data.Function (on)", \t -> do
	text t "* 試してみる", \t -> do
	itext t 0 "% ghci Tools.hs", \t -> do
	itext t 0 "*Tools> maximumBySnd [(3, 8), (2, 5), (4, 9), (5, 2)]", \t -> do
	itext t 0 "(4,9)"
 ]

beginModule :: Page
beginModule = [\t -> do
	writeTopTitle t "AIモジュール"
	text t "", \t -> do
	text t "* AI.hsを作成し以下を書き込もう", \t -> do
	itext t 1 "module AI (", \t -> do
	itext t 1 ") where", \t -> do
	itext t 1 ""
	itext t 1 "import Game (", \t -> do
	itext t 2 "Game, Turn(..), X(..), Y(..), Disk(..),"
	itext t 2 "turn, disks, placeable, initGame, nextGame)", \t -> do
	itext t 1 "import Tools (flipEnum, forMaybe, maximumBySnd)", \t -> do
	text t "* 読み込んでおこう", \t -> do
	itext t 1 "*Tools> :load AI.hs", \t -> do
	itext t 1 "*AI>"
 ]

aboutScore :: Page
aboutScore = [\t -> do
	writeTopTitle t "スコア"
	text t "", \t -> do
	text t "* まずはマスごとにスコアを返す関数を作ろう", \t -> do
	text t "* スコアのつけかたを前半と後半で変えるので", \t -> do
	itext t 1 "- スコア表をさしかえられるようにしよう", \t -> do
	text t "* 盤の対称性を利用するとその表は10ヶ所のみ指定すれば良い", \t -> do
	text t "* 対称性には以下の3つがある",\t -> do
	itext t 1 "- X方向の対称性: Xの値をひっくり返しても同じ", \t -> do
	itext t 1 "- Y方向の対称性: Yの値をひっくり返しても同じ", \t -> do
	itext t 1 "- XYの対称性: XとYの値をいれかえても同じ"
 ]

aboutScore2 :: Page
aboutScore2 = [\t -> do
	writeTopTitle t "対称性を利用した変換"
	text t "", \t -> do
	text t "* それぞれの方向での対称性を利用した変換関数を作ろう", \t -> do
	text t "* 以下をAI.hsに書き込もう", \t -> do
	itext t 1 "flipXY, flipX, flipY :: (X, Y) -> (X, Y)", \t -> do
	itext t 1 "flipXY (x, y) =", \t -> do
	itext t 2 "(toEnum $ fromEnum y, toEnum $ fromEnum x)", \t -> do
	itext t 1 "flipX = first flipEnum", \t -> do
	itext t 1 "flipY = second flipEnum", \t -> do
	text t "* first, secondはそれぞれ"
	itext t 1 "タプルの第1, 第2要素に対して関数を適用する関数", \t -> do
	text t "* Control.Arrowモジュールにあるのでモジュール宣言の下に", \t -> do
	itext t 1 "import Control.Arrow (first, second)"
 ]

aboutScore3 :: Page
aboutScore3 = [\t -> do
	writeTopTitle t "表を参照しスコアを返す"
	text t "", \t -> do
	text t "* 表の表現を決めよう。以下をAI.hsに書き込もう", \t -> do
	itext t 1 "type Table = ((X, Y), Int)", \t -> do
	text t "* 表を参照しスコアを返す関数を作る", \t -> do
	text t "* 以下をAI.hsに書き込もう", \t -> do
	itext t 0 "score :: Table -> (X, Y) -> Int", \t -> do
	itext t 0 "score t pos@(x, y)", \t -> do
	itext t 1 "| x > D = score t $ flipX pos", \t -> do
	itext t 1 "| y > Y4 = score t $ flipY pos", \t -> do
	itext t 1 "| fromEnum x < fromEnum y = score t $ flipXY pos", \t -> do
	itext t 0 "score t pos = case lookup pos t of", \t -> do
	itext t 1 "Just p -> p", \t -> do
	itext t 1 "_ -> error \"bad table\""
 ]

aboutScore4 :: Page
aboutScore4 = [\t -> do
	writeTopTitle t "表を参照しスコアを返す"
	text t "", \t -> do
	text t "* score関数は以下のようなことをしている", \t -> do
	itext t 1 "- xがDより大きければxの値をひっくりかえす", \t -> do
	itext t 1 "- yがY4より大きければyの値をひっくりかえす", \t -> do
	itext t 1 "- xよりもyが大きければxとyをいれかえる", \t -> do
	itext t 1 "- lookup関数で表の(x, y)に対応する値を探す", \t -> do
	text t "* 表で定義する必要があるのは", \t -> do
	itext t 1 "- xがD以下でyがY4以下でかつyがx以下の位置"
 ]

aboutScore5 :: Page
aboutScore5 = [\t -> do
	writeTopTitle t "表を作成する"
	text t "", \t -> do
	text t "* 表を作成するにはどこに置くとどの程度有利かを考える", \t -> do
	text t "* とりあえず角のスコアはかなり高いと考えられる", \t -> do
	text t "* それらを含めたスコアのマッピングを示す", \t -> do
	text t "* このマッピングはネットで拾ってきた"
 ]

scoreTable1 :: Page
scoreTable1 = pointPage point1

pointPage :: Table -> Page
pointPage tbl = [\t -> do
	writeTopTitle t "スコアのマッピング", \t -> do
	rtGoto t 95 85
	writeList t 40 $ map (: []) ['A' .. 'H']
	rtGoto t 55 110
	setheading t (- 90)
	writeList t 30 $ map (: []) ['1' .. '8']
	hideturtle t
	flushoff t
	pensizeRt t 1
	rtGoto t 80 90
	grid t ((show .) . tbl) 40 30 8 8
	flushon t
	showturtle t, \t -> do
	text t "* 対象性を考えると10ヶ所を設定すれば良い", \t -> do
	speed t "fast"
	rtGoto t 80 90
	gridTriangle t (\x y -> show $ tbl (x + 4) (y + 4)) 40 30 4 4
	speed t "slow"
 ]

type Table = Int -> Int -> Int

gridTriangle :: Turtle -> (Int -> Int -> String) -> Double -> Double -> Int -> Int -> IO ()
gridTriangle _ _ _ _ _ 0 = return ()
gridTriangle t s w h nx ny = do
	gridTriangle1 t (flip s ny) w h nx
	setheading t 0
	backwardRt t (fromIntegral (nx - 1) * w)
	setheading t (- 90)
	forwardRt t h
	gridTriangle t (\x y -> s x y) w h (nx - 1) (ny - 1)

gridTriangle1 :: Turtle -> (Int -> String) -> Double -> Double -> Int -> IO ()
gridTriangle1 _ _ _ _ 0 = return ()
gridTriangle1 t s w h n = do
	setheading t 0
	pencolor t "red"
	beginfill t
	replicateM_ 2 $ do
		forwardRt t w
		right t 90
		forwardRt t h
		right t 90
	endfill t
	pencolor t "black"
	pendown t
	replicateM_ 2 $ do
		forwardRt t w
		right t 90
		forwardRt t h
		right t 90
	penup t
	forwardRt t ((w - fromIntegral (length $ s n) * 8) / 2)
	setheading t (- 90)
	forwardRt t (h * 3 / 4)
	writeRt t $ s n
	backwardRt t (h * 3 / 4)
	setheading t 0
	backwardRt t ((w - fromIntegral (length $ s n) * 8) / 2)
	forwardRt t w
	gridTriangle1 t s w h (n - 1)

point1 :: Int -> Int -> Int
point1 x y
	| x > 4 = point1 (8 - x + 1) y
	| y > 4 = point1 x (8 - y + 1)
	| x < y = point1 y x
point1 1 1 = 30
point1 2 1 = -12
point1 2 2 = -15
point1 3 1 = 0
point1 3 2 = -3
point1 3 3 = 0
point1 4 1 = -1
point1 4 2 = -3
point1 4 3 = -1
point1 4 4 = -1
point1 x y = error $ "bad square (" ++ show x ++ ", " ++ show y ++ ")"

point2 :: Int -> Int -> Int
point2 x y
	| x > 4 = point2 (8 - x + 1) y
	| y > 4 = point2 x (8 - y + 1)
	| x < y = point2 y x
point2 1 1 = 120
point2 2 1 = -20
point2 2 2 = -40
point2 3 1 = 20
point2 3 2 = -5
point2 3 3 = 15
point2 4 1 = 5
point2 4 2 = - 5
point2 4 3 = 3
point2 4 4 = 3
point2 x y = error $ "bad square (" ++ show x ++ ", " ++ show y ++ ")"

writeList :: Turtle -> Double -> [String] -> IO ()
writeList _ _ [] = return ()
writeList t d (s : ss) = do
	writeRt t s
	forwardRt t d
	writeList t d ss

grid :: Turtle -> (Int -> Int -> String) -> Double -> Double -> Int -> Int -> IO ()
grid _ _ _ _ _ 0 = return ()
grid t s w h nx ny = do
	grid1 t (flip s ny) w h nx
	backwardRt t (w * fromIntegral nx)
	setheading t (- 90)
	forwardRt t h
	grid t s w h nx (ny - 1)

grid1 :: Turtle -> (Int -> String) -> Double -> Double -> Int -> IO ()
grid1 _ _ _ _ 0 = return ()
grid1 t s w h n = do
	setheading t 0
	pendown t
	replicateM_ 2 $ do
		forwardRt t w
		right t 90
		forwardRt t h
		right t 90
	penup t
	forwardRt t ((w - fromIntegral (length $ s n) * 8) / 2)
	setheading t (- 90)
	forwardRt t (h * 3 / 4)
	writeRt t $ s n
	backwardRt t (h * 3 / 4)
	setheading t 0
	backwardRt t ((w - fromIntegral (length $ s n) * 8) / 2)
	forwardRt t w
	grid1 t s w h (n - 1)

aboutTable :: Page
aboutTable = [\t -> do
	writeTopTitle t "表を作成", \t -> do
	text t "* 以下をAI.hsに書き込もう", \t -> do
	itext t 1 "table2 :: Table", \t -> do
	itext t 1 "table2 = [", \t -> do
	itext t 2 "((A, Y1), 120),", \t -> do
	itext t 2 "((B, Y1), -20),", \t -> do
	itext t 2 "((B, Y2), -40),", \t -> do
	itext t 2 "((C, Y1),  20),", \t -> do
	itext t 2 "((C, Y2),  -5),", \t -> do
	itext t 2 "((C, Y3),  15),", \t -> do
	itext t 2 "((D, Y1),   5),", \t -> do
	itext t 2 "((D, Y2),  -5),", \t -> do
	itext t 2 "((D, Y3),   3),", \t -> do
	itext t 2 "((D, Y4),   3) ]"
 ]

aboutTable2 :: Page
aboutTable2 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*AI> :reload", \t -> do
	itext t 1 "*AI> score table2 (A, Y1)", \t -> do
	itext t 1 "120", \t -> do
	itext t 1 "*AI> score table2 (H, Y8)", \t -> do
	itext t 1 "120", \t -> do
	itext t 1 "*AI> :set -XTupleSections", \t -> do
	itext t 1 "*AI> map (score table2 . (C ,)) [Y1 .. y8]", \t -> do
	itext t 1 "[20, -5, 15, 3, 3, 15, -5, 20]", \t -> do
	itext t 1 "*AI> map (score table2 . (B ,)) [Y1 .. y8]", \t -> do
	itext t 1 "[-20, -40, -5, -5, -5, -5, -40, -20]"
 ]

aboutTable3 :: Page
aboutTable3 = [\t -> do
	writeTopTitle t "表の特徴"
	text t "", \t -> do
	text t "* :set -XTupleSectionsは拡張機能を有効化したということ", \t -> do
	itext t 1 "- (3 ,)のような形の関数を使えるようにする", \t -> do
	itext t 1 "- (3 ,)は(\\x -> (3, x))ということ", \t -> do
	itext t 2 "(3 ,) 2 == (3, 2)", \t -> do
	text t "* 今見てきた表は全体として正のスコアが多くなっている", \t -> do
	text t "* つまりできるだけ多くの石を取ったほうがスコアが高くなる", \t -> do
	text t "* しかし、オセロでは序盤で石を多く取るのは良くない", \t -> do
	text t "* よって、このスコア表は後半で使うことにする", \t -> do
	text t "* 前半ではより負のスコアの多い表を使おう", \t -> do
	text t "* 前半用の表を次に示す"
 ]

scoreTable2 :: Page
scoreTable2 = pointPage point2

aboutTable4 :: Page
aboutTable4 = [\t -> do
	writeTopTitle t "表を作成", \t -> do
	text t "* 以下をAI.hsに書き込もう", \t -> do
	itext t 1 "table2 :: Table", \t -> do
	itext t 1 "table2 = [", \t -> do
	itext t 2 "((A, Y1),  30),", \t -> do
	itext t 2 "((B, Y1), -12),", \t -> do
	itext t 2 "((B, Y2), -15),", \t -> do
	itext t 2 "((C, Y1),   0),", \t -> do
	itext t 2 "((C, Y2),  -3),", \t -> do
	itext t 2 "((C, Y3),   0),", \t -> do
	itext t 2 "((D, Y1),  -1),", \t -> do
	itext t 2 "((D, Y2),  -3),", \t -> do
	itext t 2 "((D, Y3),  -1),", \t -> do
	itext t 2 "((D, Y4),  -1) ]"
 ]

aboutEvaluate :: Page
aboutEvaluate = [\t -> do
	writeTopTitle t "ゲームを評価する関数"
	text t "", \t -> do
	text t "* マスごとのスコアを返す関数が作れた", \t -> do
	text t "* ゲームのその時点でのスコアを計算する関数を作ろう", \t -> do
	text t "* 第一引数をマスからスコアを返す関数にする", \t -> do
	itext t 1 "((X, Y) -> Int) -> Disk -> Game -> Int", \t -> do
	text t "* ゲームの前半と後半で計算法を変えたい", \t -> do
	text t "* ゲームの進行度を置いてある石の数で評価するとすると", \t -> do
	text t "* Intによって計算法を変えることになるので", \t -> do
	itext t 1 "(Int -> (X, Y) -> Int) -> Disk -> Game -> Int"
 ]

aboutEvaluate2 :: Page
aboutEvaluate2 = [\t -> do
	writeTopTitle t "ゲームを評価する関数"
	text t "", \t -> do
	text t "* 自分の石と相手の石を分ける関数が必要だ", \t -> do
	text t "* 以下をAI.hsに書き込もう", \t -> do
	itext t 1 "divide :: Disk -> Game -> ([(X, Y)], [(X, Y)])", \t -> do
	itext t 1 "divide d = map fst *** map fst .", \t -> do
	itext t 2 "partition ((== d) . snd) . disks", \t -> do
	text t "* partitionは以下の2つのリストのタプルを返す", \t -> do
	itext t 1 "- 条件を満たすものを集めたリスト", \t -> do
	itext t 1 "- 条件を満たさないものを集めたリスト", \t -> do
	text t "* (***)はタプルの第1, 第2要素にそれぞれの関数を適用する", \t -> do
	text t "* map fstで[((X, Y), Disk)]を[(X, Y)]にしている"
 ]

aboutEvaluate3 :: Page
aboutEvaluate3 = [\t -> do
	writeTopTitle t "ゲームの段階を返す関数"
	text t "", \t -> do
	text t "* 今がゲームの序盤か終盤か等を判断する", \t -> do
	text t "* 簡易な方法を採り、盤面の石の数で判断するようにする", \t -> do
	text t "* 以下をAI.hsに書き込もう", \t -> do
	itext t 1 "phase :: Game -> Int", \t -> do
	itext t 1 "phase = length . disks"
 ]

aboutEvaluate4 :: Page
aboutEvaluate4 = [\t -> do
	writeTopTitle t "ゲームを評価する関数"
	text t "", \t -> do
	text t "* スコアを返す関数を使ってゲームを評価する関数を作る関数", \t -> do
	text t "* 以下をAI.hsに書き込もう", \t -> do
	itext t 0 "evaluateWith ::"
	itext t 1 "(Int -> (X, Y) -> Int) -> Disk -> Game -> Int", \t -> do
	itext t 0 "evaluateWith scr d g = ss me - ss you", \t -> do
	itext t 1 "where", \t -> do
	itext t 1 "ss = sum . map (scr $ phase g)", \t -> do
	itext t 1 "(me, you) = divide d g", \t -> do
	text t "* (scr $ phase g)でその段階でのスコア関数を得る", \t -> do
	text t "* sum . map (...)はすべての石にスコア関数を適用し", \t -> do
	itext t 1 "- その総和を求めている"
 ]

aboutEvaluate5 :: Page
aboutEvaluate5 = [\t -> do
	writeTopTitle t "ゲームを評価する関数"
	text t "", \t -> do
	text t "* これでゲームを評価する関数が作れる", \t -> do
	text t "* 以下をAI.hsに書き込もう", \t -> do
	itext t 1 "evaluate :: Disk -> Game -> Int", \t -> do
	itext t 1 "evaluate = evaluateWith $ \\p ->", \t -> do
	itext t 2 "score $ if p < 32 then table1 else table2", \t -> do
	text t "* 石が盤面上に32未満のときはtable1を使い", \t -> do
	itext t 1 "32以上のときはtable2を使うようにした"
 ]

aboutEvaluate6 :: Page
aboutEvaluate6 = [\t -> do
	writeTopTitle t "ゲームの終了時に評価する関数"
	text t "", \t -> do
	text t "* ゲーム終了時にはどのマスも同じスコアと考えられる", \t -> do
	text t "* またゲーム終了時の評価が途中よりも重要なので", \t -> do
	text t "* 以下をAI.hsに書き込もう", \t -> do
	itext t 1 "evaluateResult :: Disk -> Game -> Int", \t -> do
	itext t 1 "evaluateResult = evaluateWith $ \\_ _ -> 1000", \t -> do
	text t "* どの位置も同じスコアなので引数を無視し", \t -> do
	text t "* 絶対値を上げるためにマスのスコアを1000とした"
 ]

aboutEvaluate7 :: Page
aboutEvaluate7 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*AI> :reload", \t -> do
	itext t 1 "*AI> evaluate Black initGame", \t -> do
	itext t 1 "0", \t -> do
	itext t 1 "*AI> let Just g = nextGame initGame (C, Y5)", \t -> do
	itext t 1 "*AI> evaluate Black g", \t -> do
	itext t 1 "-3"
 ]
