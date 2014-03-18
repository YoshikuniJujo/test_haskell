import Lecture

subtitle :: String
subtitle = "第20回 まとめ:オセロ(盤の定義)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, aboutModules,
	aboutTools, aboutTools2, aboutTools3, aboutTools4, aboutTools5,
	aboutTools6, aboutTools7, aboutTools8, aboutTools9, aboutBoard,
	aboutDisk, aboutSquare, aboutSquare2, aboutTypeBoard, aboutTypeBoard2,
	aboutTypeBoard3, aboutXY, aboutComprehension,
	aboutDirection, aboutDirection2, aboutDirection3,
	aboutModuleBoard, aboutAbstraction,
	aboutLowest, aboutLowest2, aboutLowest3, aboutLowest4, aboutLowest5,
	aboutMiddle, aboutMiddle2, aboutMiddle3, aboutMiddle4, aboutMiddle5,
	aboutUpper, aboutUpper2, aboutUpper3,
	aboutBoardExport, tryBoard,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellの基本をだいたい学んだ", \t -> do
	text t "* 最後にオセロを作って終わりにしよう", \t -> do
	text t "* 難しい部分があるかもしれないが", \t -> do
	itext t 1 "- 全体の雰囲気がつかめれば良いとする", \t -> do
	text t "* 4回に分けて作っていこう", \t -> do
	text t "* 今回は盤を定義していこう"
 ]

aboutModules :: Page
aboutModules = [\t -> do
	writeTopTitle t "モジュール"
	text t "", \t -> do
	text t "* 以下のようなモジュールに分ける", \t -> do
	itext t 1 "Main, Window, AI, Game, Board, Tools", \t -> do
	text t "* MainはWindowの関数を呼び出すだけ", \t -> do
	text t "* WindowはAIとGameの関数をGUI上で結合する", \t -> do
	text t "* AIは与えられたゲームの状態からコンピュータの手を計算", \t -> do
	text t "* Gameはゲームの状態を表現する型と操作する関数を定義", \t -> do
	text t "* Boardは盤の状態を表現する型と操作する関数を定義", \t -> do
	text t "* Toolsではより一般的に使えそうな関数を定義する", \t -> do
	text t "* 今回はBoardとToolsの一部を作ろう"
 ]

aboutTools :: Page
aboutTools = [\t -> do
	writeTopTitle t "Tools", \t -> do
	text t "* lectures/othelloディレクトリを作成しよう", \t -> do
	text t "* Boardモジュールで使うより一般的な道具を作成", \t -> do
	text t "* Tools.hsに以下を書き込もう", \t -> do
	itext t 1 "module Tools (", \t -> do
	itext t 1 ") where", \t -> do
	text t "* 対話環境でテストしながら作っていくので", \t -> do
	itext t 1 "% ghci Tools.hs", \t -> do
	text t "* Boardモジュールで使う道具は以下の4つ", \t -> do
	itext t 1 "scc: 次の値を返す、最大値ではNothingを", \t -> do
	itext t 1 "prd: 前の値を返す、最小値ではNothingを", \t -> do
	itext t 1 "foldlMaybe: foldlと同様だがJustがなければNothing", \t -> do
	itext t 1 "modifyList: リストの要素のひとつに関数を適用する"
 ]

aboutTools2 :: Page
aboutTools2 = [\t -> do
	writeTopTitle t "scc, prd", \t -> do
	text t "* 次の値や前の値を返す関数succ, predがある", \t -> do
	text t "* これらの関数は結果が最大値や最小値を越えるとエラー", \t -> do
	text t "* エラーよりもNothingを返すほうが扱いやすい", \t -> do
	text t "* Tools.hsに以下を書き込もう", \t -> do
	itext t 0 "scc, prd :: (Ord a, Enum a, Bounded a) => a -> Maybe a", \t -> do
	itext t 0 "scc x"
	preLine t
	itext t 1 "| x < maxBound = Just $ succ x", \t -> do
	itext t 1 "| otherwise = Nothing", \t -> do
	itext t 0 "prd x"
	preLine t
	itext t 1 "| x > minBound = Just $ pred x", \t -> do
	itext t 1 "| otherwise = Nothing", \t -> do
	text t "* succやpredはEnumのクラス関数", \t -> do
	text t "* Boundedは最大値と最小値を持つということ", \t -> do
	text t "* Ordは大小比較可能ということ"
 ]


aboutTools3 :: Page
aboutTools3 = [\t -> do
	writeTopTitle t "foldlMaybe"
	text t "", \t -> do
	text t "* オセロでは8方向のどこかで相手の石を取れないと置けない", \t -> do
	text t "* 結果の状態は8方向の取れる石すべてを取った状態", \t -> do
	text t "* 与えられた方向で、取れるときは取った状態を返し", \t -> do
	itext t 1 "取れないときはNothingを返す関数があったとすると", \t -> do
	text t "* ひとつでもJustがあればJustを返す方向すべての関数を適用", \t -> do
	text t "* Justがなければ全体としてもNothingとする", \t -> do
	text t "* つまり以下の関数があれば都合がいい", \t -> do
	itext t 0 "foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a", \t -> do
	text t "* aを状態としbを方向と考えよう", \t -> do
	text t "* 状態を方向を変えながら変化させていく", \t -> do
	text t "* 変化がない(つまりJustがない)ときはNothingとする"
 ]

aboutTools4 :: Page
aboutTools4 = [\t -> do
	writeTopTitle t "foldlMaybe"
	text t "", \t -> do
	text t "* foldlMaybe op x (y : ys)としたときに", \t -> do
	itext t 1 "- x `op` yがNothingならばそのyは無視する", \t -> do
	itext t 1 "- Just x'ならばx'を新しい値として続ける", \t -> do
	text t "* しかし、これだとすべてNothingの場合には", \t -> do
	itext t 1 "単にもともとの値xを返すということになる", \t -> do
	text t "* すべてNothingの場合にはNothingにしたい", \t -> do
	arrowIText t 1 "Justの有無を保存するBool値が必要", \t -> do
	itext t 0 "foldlMaybeBool ::", \t -> do
	itext t 1 "Bool -> (a -> b -> Maybe a) -> a -> [b] -> Maybe a", \t -> do
	text t "* Bool値はJustがあった場合にTrue、無かった場合にFalse"
 ]

aboutTools5 :: Page
aboutTools5 = [\t -> do
	writeTopTitle t "foldlMaybe"
	text t "", \t -> do
	text t "* まずはすべての値を使い終わったとき", \t -> do
	itext t 1 "- Justがあったなら現在の値を返し", \t -> do
	itext t 1 "- なかったならNothingを返す", \t -> do
	itext t 1 "foldlMaybeBool True _ x [] = Just x", \t -> do
	itext t 1 "foldlMaybeBool False _ _ [] = Nothing", \t -> do
	text t "* 値が残っている場合にはx `op` yが", \t -> do
	itext t 1 "- Just x'ならばBool値をTrueにしてxをx'にする", \t -> do
	itext t 1 "- Nothingならyは無視しBool値もxもそのままにする", \t -> do
	itext t 1 "foldlMaybeBool j op x (y : ys) = case x `op` y of", \t -> do
	itext t 2 "Just x' -> foldlMaybeBool True op x' ys", \t -> do
	itext t 2 "_ -> foldlMaybeBool j op x ys"
 ]

aboutTools6 :: Page
aboutTools6 = [\t -> do
	writeTopTitle t "foldlMaybe", \t -> do
	text t "* foldlMaybeはfoldlMaybeBoolに初期値としてFalseを与える", \t -> do
	text t "* 以下をTools.hsに書き込もう", \t -> do
	itext t 0 "foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a", \t -> do
	itext t 0 "foldlMaybe = foldlMaybeBool False", \t -> do
	itext t 0 ""
	itext t 0 "foldlMaybeBool ::"
	itext t 1 "Bool -> (a -> b -> Maybe a) -> a -> [b] -> Maybe a", \t -> do
	itext t 0 "foldlMaybeBool True _ x [] = Just x", \t -> do
	itext t 0 "foldlMaybeBool False _ _ [] = Nothing", \t -> do
	itext t 0 "foldlMaybeBool j op x (y : ys) = case x `op` y of", \t -> do
	itext t 1 "Just x' -> foldlMaybeBool True op x' ys", \t -> do
	itext t 1 "_ -> foldlMaybeBool j op x ys"
 ]

aboutTools7 :: Page
aboutTools7 = [\t -> do
	writeTopTitle t "modifyList"
	text t "", \t -> do
	text t "* modifyListは要素のひとつを変化させたリストを返す", \t -> do
	text t "* 前の部分、その要素、後の部分に分けて", \t -> do
	itext t 1 "- 前の部分、変化させた要素、後の部分を結合させる", \t -> do
	text t "* Tools.hsに書き込もう", \t -> do
	itext t 0 "modifyList :: [a] -> Int -> (a -> a) -> [a]", \t -> do
	itext t 0 "modifyList xs n f ="
	itext t 1 "take n xs ++ [f $ xs !! n] ++ drop (n + 1) xs", \t -> do
	text t "* takeは与えられた数だけ要素を取り出す", \t -> do
	text t "* dropは与えられた数だけ要素を捨てる", \t -> do
	text t "* (!!)は与えられたインデックスで要素を取り出す"
 ]

aboutTools8 :: Page
aboutTools8 = [\t -> do
	writeTopTitle t "Tools"
	text t "", \t -> do
	text t "* 作った関数をエクスポートリストに追加しよう", \t -> do
	itext t 1 "module Tools ("
	itext t 2 "scc, prd, foldlMaybe, modifyList"
	itext t 1 ") where", \t -> do
	text t "* ケアレスミスをチェックするためにreloadしてみる", \t -> do
	itext t 1 "*Tools> :reload"
 ]

aboutTools9 :: Page
aboutTools9 = [\t -> do
	writeTopTitle t "Tools"
	text t "", \t -> do
	text t "* いくつか試してみる", \t -> do
	itext t 1 "*Tools> scc 'c'", \t -> do
	itext t 1 "Just 'd'", \t -> do
	itext t 1 "*Tools> scc '\\0'", \t -> do
	itext t 1 "Just '\\SOH'", \t -> do
	itext t 1 "*Tools> prd '\\0'", \t -> do
	itext t 1 "Nothing", \t -> do
	itext t 1 "*Tools> :m + Data.Char", \t -> do
	itext t 1 "*Tools Data.Char> modifyList \"Hello\" 2 toUpper", \t -> do
	itext t 1 "\"HeLlo\""
 ]

aboutBoard :: Page
aboutBoard = [\t -> do
	writeTopTitle t "Board"
	text t "", \t -> do
	text t "* 一般的な道具ができたので次はBoardモジュール", \t -> do
	text t "* まずはモジュール宣言を書き込む", \t -> do
	itext t 1 "module Board ("
	itext t 1 ") where", \t -> do
	text t "* エクスポートリストは今は空", \t -> do
	text t "* ()のなかにエクスポートする関数等を追加していく", \t -> do
	text t "* 対話環境でテストしながら作っていこう", \t -> do
	itext t 1 "% ghci Board.hs", \t -> do
	itext t 1 "*Board> "
 ]

aboutDisk :: Page
aboutDisk = [\t -> do
	writeTopTitle t "Disk"
	text t "", \t -> do
	text t "* 石を表すデータ構造を作る", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 1 "data Disk = Black | White deriving (Eq, Show)", \t -> do
	itext t 1 "rev :: Disk -> Disk", \t -> do
	itext t 1 "rev Black = White", \t -> do
	itext t 1 "rev White = Black", \t -> do
	text t "* 石には黒と白がありそれぞれ互いの裏になる"
 ]

aboutSquare :: Page
aboutSquare = [\t -> do
	writeTopTitle t "Square"
	text t "", \t -> do
	text t "* 次は盤のマスを表すデータ構造を作る", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 1 "data Square = Disk { disk :: Disk } | Empty"
	itext t 2 "deriving (Eq, Show)", \t -> do
	text t "* マスは石が置いてあるか、または空", \t -> do
	text t "* Disk { disk :: Disk }はDisk Diskと同じ", \t -> do
	text t "* Disk型を取る値構築子Diskを定義している", \t -> do
	text t "* 同時にSquareからDiskを取り出すdisk関数を定義している"
 ]

aboutSquare2 :: Page
aboutSquare2 = [\t -> do
	writeTopTitle t "Square"
	text t "", \t -> do
	text t "* 石があるかどうか調べる関数を作る", \t -> do
	text t "* また、石を変化させる関数も作ろう", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 1 "isDisk :: Square -> Bool", \t -> do
	itext t 1 "isDisk (Disk _) = True", \t -> do
	itext t 1 "isDisk _ = False"
	itext t 1 "", \t -> do
	itext t 1 "modifyDisk :: (Disk -> Disk) -> Square -> Square", \t -> do
	itext t 1 "modifyDisk f (Disk d) = Disk $ f d"
	itext t 1 "modifyDisk _ s = s"
 ]

aboutTypeBoard :: Page
aboutTypeBoard = [\t -> do
	writeTopTitle t "Board", \t -> do
	text t "* マスの横の並びを行とすると盤は行を集めたもの", \t -> do
	text t "* 対話環境用にshow関数を定義しておく", \t -> do
	text t "* 盤の初期値も定義しておこう", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 1 "newtype Board = Board [[Square]]", \t -> do
	itext t 1 "instance Show Board where", \t -> do
	itext t 2 "show (Board b) ="
	itext t 3 "unlines $ map (concatMap sd) b", \t -> do
	itext t 3 "where", \t -> do
	itext t 3 "sd (Disk Black) = \"*|\"", \t -> do
	itext t 3 "sd (Disk White) = \"O|\"", \t -> do
	itext t 3 "sd Empty = \"_|\""
 ]

aboutTypeBoard2 :: Page
aboutTypeBoard2 = [\t -> do
	writeTopTitle t "initBoard", \t -> do
	itext t 1 "initBoard :: Board", \t -> do
	itext t 1 "initBoard = Board $ map (map c2d) [", \t -> do
	itext t 2 "\"________\",", \t -> do
	itext t 2 "\"________\",", \t -> do
	itext t 2 "\"________\",", \t -> do
	itext t 2 "\"___O*___\",", \t -> do
	itext t 2 "\"___*O___\",", \t -> do
	itext t 2 "\"________\",", \t -> do
	itext t 2 "\"________\",", \t -> do
	itext t 2 "\"________\" ]", \t -> do
	itext t 2 "where"
	preLine t
	itext t 3 "c2d '_' = Empty", \t -> do
	itext t 3 "c2d '*' = Disk Black", \t -> do
	itext t 3 "c2d 'O' = Disk White"
 ]

aboutTypeBoard3 :: Page
aboutTypeBoard3 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Tools> :load Board.hs", \t -> do
	itext t 1 "*Board> initBoard", \t -> do
	itext t 1 "_|_|_|_|_|_|_|_|"
	itext t 1 "..."
	itext t 1 "_|_|_|O|*|_|_|_|"
	itext t 1 "_|_|_|*|O|_|_|_|"
	itext t 1 "...", \t -> do
	text t "* 盤の初期状態が表示される"
 ]

aboutXY :: Page
aboutXY = [\t -> do
	writeTopTitle t "X, Y"
	text t "", \t -> do
	text t "* 盤上のマスの位置を示す型X, Yを定義する", \t -> do
	text t "* またそれを使って「全てのマス」を定義する", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 0 "data X = A | B | C | D | E | F | G | H", \t -> do
	itext t 1 "deriving (Eq, Ord, Enum, Bounded, Show)", \t -> do
	itext t 0 "data Y = Y1 | Y2 | Y3 | Y4 | Y5 | Y6 | Y7 | Y8", \t -> do
	itext t 1 "deriving (Eq, Ord, Enum, Bounded, Show)"
	itext t 0 "", \t -> do
	itext t 0 "allSquares :: [(X, Y)]", \t -> do
	itext t 0 "allSquares = [ (x, y) | x <- [A .. H], y <- [Y1 .. Y8] ]"
 ]

aboutComprehension :: Page
aboutComprehension = [\t -> do
	writeTopTitle t "リストの内包表現"
	text t "", \t -> do
	text t "* Haskellには「リストの内包表現」という構文糖がある", \t -> do
	text t "* 以下ですべてのx, yの組み合わせのリストとなる", \t -> do
	itext t 1 "[ (x, y) | x <- [リスト1], y <- [リスト2] ]", \t -> do
	text t "* また途中にBool値を入れることで値をしぼりこめる", \t -> do
	itext t 1 "[ x | x <- [リスト1], test x ]", \t -> do
	text t "* 値を束縛することもできる", \t -> do
	itext t 1 "[ v | x <- [リスト1], let v = f x, test v ]"
 ]

aboutDirection :: Page
aboutDirection = [\t -> do
	writeTopTitle t "Direction"
	text t "", \t -> do
	text t "* オセロには石を取っていく方向も必要", \t -> do
	text t "* 方向とは(X, Y)を変化させる規則とも考えられるので", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 1 "type Direction = (X -> Maybe X, Y -> Maybe Y)", \t -> do
	text t "* 一番端では動けなくなるので、それをMaybeで表現した", \t -> do
	text t "* 方向の表現には以下を使う", \t -> do
	itext t 1 "prd : 負の向きへの動き", \t -> do
	itext t 1 "Just: その座標は動かさない", \t -> do
	itext t 1 "scc : 正の向きへの動き"
 ]

aboutDirection2 :: Page
aboutDirection2 = [\t -> do
	writeTopTitle t "Direction"
	text t "", \t -> do
	text t "* prd, sccを使うので以下をモジュール宣言の下に", \t -> do
	itext t 1 "import Tools(prd, scc)", \t -> do
	text t "* (X, Y)を与えられた方向に動かす関数", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 1 "move :: Direction -> (X, Y) -> Maybe (X, Y)", \t -> do
	itext t 1 "move (dx, dy) (x, y) = do", \t -> do
	itext t 2 "x' <- dx x", \t -> do
	itext t 2 "y' <- dy y", \t -> do
	itext t 2 "return (x', y')"
 ]

aboutDirection3 :: Page
aboutDirection3 = [\t -> do
	writeTopTitle t "Direction"
	text t "", \t -> do
	text t "* 8方向すべてを表す値を定義する", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 1 "allDirs :: [Direction]", \t -> do
	itext t 1 "allDirs = [", \t -> do
	itext t 2 "( prd,  prd), (Just,  prd), ( scc,  prd),", \t -> do
	itext t 2 "( prd, Just),               ( scc, Just),", \t -> do
	itext t 2 "( prd,  scc), (Just,  scc), ( scc,  scc) ]"
 ]

aboutModuleBoard :: Page
aboutModuleBoard = [\t -> do
	writeTopTitle t "Boardモジュールの最終目標"
	text t "", \t -> do
	text t "* これまで定義した型は以下の通り", \t -> do
	itext t 1 "Disk, Square, Board, X, Y, Direction", \t -> do
	text t "* これらを使って盤に関する操作を作っていく", \t -> do
	text t "* 最終的には盤に対して以下ができれば良い", \t -> do
	itext t 1 "- 盤のどこに石が置いてあるか調べる", \t -> do
	itext t 1 "- それぞれの石がどこに置けるかを調べる", \t -> do
	itext t 1 "- 盤に石を置き取れる石はすべて取る", \t -> do
	text t "* 盤に対するそれ以外の操作は公開しない", \t -> do
	arrowIText t 1 "盤がおかしな状態になることはない", \t -> do
	text t "* ただし、Boardでは、"
	itext t 1 "黒と白が「順番に」置くというルールは定義しない"
 ]

aboutAbstraction :: Page
aboutAbstraction = [\t -> do
	writeTopTitle t "抽象化の層", \t -> do
	text t "* 抽象化の層を重ねていく", \t -> do
	text t "* 一番下の層では以下の関数を定義する", \t -> do
	itext t 1 "get: マスの状態を入手", \t -> do
	itext t 1 "put: マスに石を置く", \t -> do
	itext t 1 "cap: マスの石を裏がえす", \t -> do
	text t "* 二番目の層では以下の関数を定義する", \t -> do
	itext t 1 "capture: すべての方向の石を裏がえす", \t -> do
	itext t 1 "put: 下の層のputをそのまま上の層へ", \t -> do
	text t "* 三番目の層はBoardで公開する関数を定義", \t -> do
	itext t 1 "disks: 盤上の石の位置を返す", \t -> do
	itext t 1 "placeable: 盤上で石を置ける位置を返す", \t -> do
	itext t 1 "place: 盤に石を置いて新しい盤を返す"
 ]

aboutLowest :: Page
aboutLowest = [\t -> do
	writeTopTitle t "最下層"
	text t "", \t -> do
	text t "* 実際の開発では"
	itext t 1 "トップダウンとボトムアップを行ったり来たりする", \t -> do
	text t "* 説明の都合上、今回はボトムアップで作っていく", \t -> do
	text t "* 一番下の層のgetを定義する", \t -> do
	text t "* 本質的には二重のリストから値を取り出すということ", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 0 "get :: Board -> (X, Y) -> Disk", \t -> do
	itext t 0 "get (Board b) (x, y) = b !! fromEnum y !! fromEnum x"
 ]

aboutLowest2 :: Page
aboutLowest2 = [\t -> do
	writeTopTitle t "最下層"
	text t "", \t -> do
	text t "* 次はputとcapを作るのだが両者には共通の構造がある", \t -> do
	text t "* 与えられた場所のSquareを変化させるという構造だ", \t -> do
	text t "* まずはそれを定義しよう", \t -> do
	text t "* ToolsのmodifyListを使うのでモジュール宣言の下に", \t -> do
	itext t 0 "import Tools (prd, scc, modifyList)", \t -> do
	text t "* これと以下を書き込もう", \t -> do
	itext t 0 "modifySquare ::", \t -> do
	itext t 1 "Board -> (Square -> Square) -> (X, Y) -> Board", \t -> do
	itext t 0 "modifySquare (Board b) f (x, y) = Board $", \t -> do
	itext t 1 "modifyList b (fromEnum y) $", \t -> do
	itext t 2 "\\l -> modifyList l (fromEnum x) f"
 ]

aboutLowest3 :: Page
aboutLowest3 = [\t -> do
	writeTopTitle t "最下層"
	text t "", \t -> do
	text t "* modifySquareの定義を再掲する", \t -> do
	itext t 1 "modifySquare (Board b) f (x, y) = Board $", \t -> do
	itext t 2 "modifyList b (fromEnum y) $", \t -> do
	itext t 3 "\\l -> modifyList l (fromEnum x) f", \t -> do
	text t "* modifyListをネストさせている", \t -> do
	text t "* 簡単に言うと「y番目のリストのx番目を変化させる」"
 ]

aboutLowest4 :: Page
aboutLowest4 = [\t -> do
	writeTopTitle t "最下層"
	text t "", \t -> do
	text t "* これを使ってputとcapを定義しよう", \t -> do
	text t "* putはマスが空ならば与えられた石を置くこと", \t -> do
	text t "* Board.hsに以下を書き込もう", \t -> do
	itext t 0 "put :: Board -> Disk -> (X, Y) -> Maybe Board", \t -> do
	itext t 0 "put b d p = case get b p of", \t -> do
	itext t 1 "Empty -> Just $ modifySquare b (const $ Disk d) p", \t -> do
	itext t 1 "_ -> Nothing", \t -> do
	text t "* まずはマスが空かどうかで場合分けする", \t -> do
	text t "* 空ならばmodifySquareに", \t -> do
	itext t 1 "「もとの値に関らず与えられた石に変える」関数を渡す"
 ]

aboutLowest5 :: Page
aboutLowest5 = [\t -> do
	writeTopTitle t "最下層"
	text t "", \t -> do
	text t "* capはマスが黒ならば白に白ならば黒にする", \t -> do
	text t "* マスが空ならばそのままにする", \t -> do
	text t "* Board.hsに以下を書き込もう", \t -> do
	itext t 1 "cap :: Board -> (X, Y) -> Board", \t -> do
	itext t 1 "cap b = modifySquare b $ modifyDisk rev", \t -> do
	text t "* マスの石を裏にする関数をmodifySquareに渡している"
 ]

aboutMiddle :: Page
aboutMiddle = [\t -> do
	writeTopTitle t "中間層", \t -> do
	text t "* 中間層がするのは", \t -> do
	itext t 1 "1. 最下層で定義したputはそのまま上部層へ公開", \t -> do
	itext t 1 "2. 8方向の石を取るcaptureを定義する", \t -> do
	text t "* 1に関してはとくに何もする必要もない", \t -> do
	text t "* captureを定義していこう", \t -> do
	text t "* まずは一方向について石を取る関数capture1を作る", \t -> do
	text t "* 例として自分が黒である場合について考えてみよう", \t -> do
	itext t 1 "- そのマスが白ならば黒にして次に進む", \t -> do
	itext t 1 "- そのマスが黒ならば今までの変化を確定する", \t -> do
	itext t 1 "- 空のマスまたは盤の端に来たら今までの変化を破棄", \t -> do
	text t "* ただし一番はじめのマスが黒だったら", \t -> do
	itext t 1 "- ひとつも取れなかったことになるので破棄される"
 ]

aboutMiddle2 :: Page
aboutMiddle2 = [\t -> do
	writeTopTitle t "中間層"
	text t "", \t -> do
	text t "* すくなくともひとつ石を取る必要があるので", \t -> do
	itext t 1 "取ったかどうかを保存するBool値が必要になる", \t -> do
	text t "* Boolを取るその関数をcapture1Boolとするとその型は", \t -> do
	itext t 0 "capture1Bool :: Bool -> Board ->"
	itext t 1 "Disk -> (X, Y) -> Direction -> Maybe Board", \t -> do
	text t "* 以下のようなアルゴリズムになるだろう(黒で取る場合)", \t -> do
	itext t 1 "- そこの石が白ならば黒にしBool値をTrueにして次へ", \t -> do
	itext t 1 "- そこの石が黒ならば", \t -> do
	itext t 2 "Bool値がTrueならば確定", \t -> do
	itext t 2 "Bool値がFalseならば破棄", \t -> do
	itext t 1 "- そこが空ならば破棄"
 ]

aboutMiddle3 :: Page
aboutMiddle3 = [\t -> do
	writeTopTitle t "中間層"
	text t "", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 0 "capture1Bool :: Bool -> Board ->"
	itext t 1 "Disk -> (X, Y) -> Direction -> Maybe Board", \t -> do
	itext t 0 "capture1Bool c b d0 p dir = case get b p of", \t -> do
	itext t 1 "Disk d -> case (d == d0, c) of", \t -> do
	itext t 2 "(False, _) -> do", \t -> do
	itext t 3 "p' <- move dir p", \t -> do
	itext t 3 "capture1Bool True (cap b p) d0 p' dir", \t -> do
	itext t 2 "(_, True) -> Just b", \t -> do
	itext t 2 "_ -> Nothing", \t -> do
	itext t 1 "_ -> Nothing"
 ]

aboutMiddle4 :: Page
aboutMiddle4 = [\t -> do
	writeTopTitle t "中間層"
	text t "", \t -> do
	text t "* capture1は初期値Falseを与える", \t -> do
	text t "* また、capture1Boolは置いた場所の隣りから始めるので", \t -> do
	itext t 1 "- あらかじめ位置をひとつ進める必要がある", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 0 "capture1 :: Board ->"
	itext t 1 "Disk -> (X, Y) -> Direction -> Maybe Board", \t -> do
	itext t 0 "capture1 b d p dir = do", \t -> do
	itext t 1 "p' <- move dir p", \t -> do
	itext t 1 "capture1Bool False b d p' dir"
 ]

aboutMiddle5 :: Page
aboutMiddle5 = [\t -> do
	writeTopTitle t "中間層"
	text t "", \t -> do
	text t "* captureはfoldlMaybeを使えば定義できる", \t -> do
	text t "* モジュール宣言の下に以下を書き込み", \t -> do
	itext t 0 "import Tools (prd, scc, modifyList, foldlMaybe)", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 0 "capture :: Board -> Disk -> (X, Y) -> Maybe Board", \t -> do
	itext t 0 "capture brd dsk pos ="
	itext t 1 "foldlMaybe (\\b -> capture1 b dsk pos) brd allDirs"
 ]

aboutUpper :: Page
aboutUpper = [\t -> do
	writeTopTitle t "上部層"
	text t "", \t -> do
	text t "* 上部層で必要なのは", \t -> do
	itext t 1 "disks: 盤から石の位置を入手する", \t -> do
	itext t 1 "placeable: 盤の石を置ける位置を調べる", \t -> do
	itext t 1 "place: 盤に実際に石を置く", \t -> do
	text t "* disksは全てのマスから石のあるマスを選べば良い", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 1 "disks :: Board -> [((X, Y), Disk)]", \t -> do
	itext t 1 "disks b = [ (p, disk s) |"
	itext t 2 "p <- allSquares, let s = get b p, isDisk s ]"
 ]

aboutUpper2 :: Page
aboutUpper2 = [\t -> do
	writeTopTitle t "上部層"
	text t "", \t -> do
	text t "* placeableはplaceを使って定義することにする", \t -> do
	text t "* まずはplaceを定義しよう", \t -> do
	text t "* その位置に与えられた色の石を置き", \t -> do
	itext t 1 "- 取れる石をすべて取れば良いので", \t -> do
	text t "* 以下をBoard.hsに書き込もう", \t -> do
	itext t 1 "place :: Board -> Disk -> (X, Y) -> Maybe Board", \t -> do
	itext t 1 "place b d pos = do", \t -> do
	itext t 2 "b' <- put b d pos", \t -> do
	itext t 2 "capture b' s pos"
 ]

aboutUpper3 :: Page
aboutUpper3 = [\t -> do
	writeTopTitle t "上部層"
	text t "", \t -> do
	text t "* placeableをplaceを使って定義する", \t -> do
	text t "* Data.MaybeモジュールのisJust関数を使うので", \t -> do
	text t "* モジュール宣言の下に以下を書き込もう", \t -> do
	itext t 1 "import Data.Maybe (isJust)", \t -> do
	text t "* placeが置けない場合にNothingを返すことを利用する", \t -> do
	text t "* Board.hsに以下を書き込もう", \t -> do
	itext t 0 "placeable :: Board -> Disk -> [(X, Y)]", \t -> do
	itext t 0 "placeable b d ="
	itext t 1 "[ p | p <- allSquares, isJust $ place b d p ]"
 ]

aboutBoardExport :: Page
aboutBoardExport = [\t -> do
	writeTopTitle t "エクスポートリスト"
	text t "", \t -> do
	text t "* 必要な関数はそろったので公開する関数を宣言しよう", \t -> do
	text t "* モジュール宣言を以下のようにしよう", \t -> do
	itext t 1 "module Board (", \t -> do
	itext t 2 "Disk(..), rev,", \t -> do
	itext t 2 "Board, initBoard, X(..), Y(..),", \t -> do
	itext t 2 "disks, placeable, place"
	itext t 1 ") where"

 ]

tryBoard :: Page
tryBoard = [\t -> do
	writeTopTitle t "試してみる", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Board> :reload", \t -> do
	itext t 1 "*Board> initBoard", \t -> do
	itext t 1 "...(盤の初期状態)", \t -> do
	itext t 1 "*Board> placeable initBoard Black", \t -> do
	itext t 1 "[(C, Y5), (D, Y4), (E, Y7), (F, Y6)]", \t -> do
	itext t 1 "*Board> place initBoard Black (D, Y4)", \t -> do
	itext t 1 "Just _|_|_|_|_|_|_|_|"
	itext t 1 "_|_|_|_|_|_|_|_|"
	itext t 1 "_|_|_|*|_|_|_|_|"
	itext t 1 "_|_|_|*|*|_|_|_|"
	itext t 1 "_|_|_|*|O|_|_|_|"
	itext t 1 "..."
 ]

beginGameModule :: Page
beginGameModule = [\t -> do
	writeTopTitle t "Gameモジュール"
	text t "", \t -> do
	text t "* Boardモジュールに足りなかったものがある", \t -> do
	text t "* それは今が黒と白のどちらのターンなのかということだ"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 盤と石そして盤上の位置を表す型を定義した", \t -> do
	text t "* 盤の操作を定義した", \t -> do
	text t "* 盤に対してできるのは以下の3つ", \t -> do
	itext t 1 "- 盤上の石の位置を調べる", \t -> do
	itext t 1 "- 盤上の置ける位置を調べる", \t -> do
	itext t 1 "- 盤上に石を置く", \t -> do
	text t "* initBoardと石と位置を表す型も公開した", \t -> do
	text t "* initBoardに石を指定の位置に置いていくことができる"
 ]
