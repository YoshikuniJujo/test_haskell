import Lecture

subtitle :: String
subtitle = "第21回 まとめ:オセロ(ゲームの定義)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, beginGameModule,
	aboutTypes, gameFunctions, gameFunctions2, gameFunctions3,
	gameFunctions4, gameFunctions5,
	publicFunctions, publicFunctions2, publicFunctions3,
	aboutGameModule,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回のBoard型には足りないところがある", \t -> do
	text t "* 今がどちらのターンなのかという情報がない", \t -> do
	text t "* Game型はターンの情報とBoard型を持つことにする"
 ]

beginGameModule :: Page
beginGameModule = [\t -> do
	writeTopTitle t "Gameモジュール"
	text t "", \t -> do
	text t "* Game.hsを作り以下を書き込もう", \t -> do
	itext t 1 "module Game ("
	itext t 1 ") where"
	itext t 1 "", \t -> do
	itext t 1 "import Board(Disk(..), rev, Board, initBoard,"
	itext t 2 "X(..), Y(..), place)", \t -> do
	itext t 1 "import qalified Board (disks, placeable, place)", \t -> do
	text t "* qualifiedをつけると完全修飾名のみでのアクセスとなる", \t -> do
	itext t 1 "Board.disks, Board.placeable, Board.place", \t -> do
	text t "* Gameモジュール内で同名の関数を定義するので"
 ]

aboutTypes :: Page
aboutTypes = [\t -> do
	writeTopTitle t "Turn型, Game型"
	text t "", \t -> do
	text t "* ゲームの状態はそれぞれの石のターンまたはゲームオーバー", \t -> do
	text t "* Game.hsに以下を書き込もう", \t -> do
	itext t 1 "data Turn = Turn Disk | GameOver deriving Show", \t -> do
	text t "* Game型はTurn型とBoard型を保持する", \t -> do
	text t "* Game.hsに以下を書き込もう", \t -> do
	itext t 1 "data Game = Game { turn :: Turn, board :: Board }"
	itext t 2 "deriving Show", \t -> do
	text t "* フィールドを取り出す関数turn, boardも同時に定義した"
 ]

gameFunctions :: Page
gameFunctions = [\t -> do
	writeTopTitle t "Game型用の関数"
	text t "", \t -> do
	text t "* Board型用の関数をGame型用に作り直す", \t -> do
	text t "* まずはdisksとplaceableから", \t -> do
	text t "* Game.hsに以下を書き込もう", \t -> do
	itext t 1 "disks :: Game -> [((X, Y), Disk)]", \t -> do
	itext t 1 "disks = Board.disk . board"
	itext t 1 "", \t -> do
	itext t 1 "placeable :: Game -> [(X, Y)]", \t -> do
	itext t 1 "placeable (Game (Turn d) b) = Board.placeable b d", \t -> do
	itext t 1 "placeable _ = []", \t -> do
	text t "* 新しいplaceableはTurn型の値から置く石を決定している"
 ]

gameFunctions2 :: Page
gameFunctions2 = [\t -> do
	writeTopTitle t "Game型用の関数"
	text t "", \t -> do
	text t "* 次にplace関数もGame用に作り直す", \t -> do
	text t "* 以下をGame.hsに書き込もう", \t -> do
	itext t 1 "place :: Game -> (X, Y) -> Maybe Game", \t -> do
	itext t 1 "place (Game t@(Turn d) b) pos ="
	itext t 2 "Game t <$> Board.place b d pos", \t -> do
	itext t 1 "place _ _ = Nothing", \t -> do
	text t "* Control.Applicativeモジュールの(<$>)を使っているので", \t -> do
	text t "* モジュール宣言の下に以下を書き加えよう", \t -> do
	itext t 1 "import Control.Applicative ((<$>))"
 ]

gameFunctions3 :: Page
gameFunctions3 = [\t -> do
	writeTopTitle t "Game型用の関数"
	text t "", \t -> do
	text t "* placeの定義の一部を再掲する", \t -> do
	itext t 1 "place (Game t@(Turn d) b) pos ="
	itext t 2 "Game t <$> Board.place b d pos", \t -> do
	text t "* まずはパターンに出てくる'@'がある", \t -> do
	itext t 1 "- これは全体をtに束縛したうえで", \t -> do
	itext t 1 "- さらにその内部にパターンマッチを行うということ", \t -> do
	text t "* 次に(<$>)関数だが、これは「中身」に関数を適用する", \t -> do
	itext t 1 "(+ 1) <$> (Just 8)", \t -> do
	arrowIText t 2 "Just 9"
 ]

gameFunctions4 :: Page
gameFunctions4 = [\t -> do
	writeTopTitle t "pass"
	text t "", \t -> do
	text t "* オセロでは置く場所がないときにパスとなるので", \t -> do
	itext t 1 "- それを関数にしておこう", \t -> do
	text t "* 以下をGame.hsに書き込もう", \t -> do
	itext t 1 "pass :: Game -> Bool", \t -> do
	itext t 1 "pass = null . placeable"
 ]

gameFunctions5 :: Page
gameFunctions5 = [\t -> do
	writeTopTitle t "flipTurn, gameOver"
	text t "", \t -> do
	text t "* Game型のTurnの値を扱う関数を作っておく", \t -> do
	text t "* 黒と白のターンを入れ換える関数と", \t -> do
	text t "* ターンをGameOverにする関数を作る", \t -> do
	text t "* 以下をGame.hsに書き込もう", \t -> do
	itext t 1 "flipTurn, gameOver :: Game -> Game", \t -> do
	itext t 1 "flipTurn (Game (Turn d) b) = Game (Turn $ rev d) b", \t -> do
	itext t 1 "flipTurn g = g", \t -> do
	itext t 1 "gameOver (Game _ b) = Game GameOver b"
 ]

publicFunctions :: Page
publicFunctions = [\t -> do
	writeTopTitle t "initGame"
	text t "", \t -> do
	text t "* ゲームは黒のターンで始まるので", \t -> do
	text t "* 以下をGame.hsに書き込もう", \t -> do
	itext t 1 "initGame :: Game", \t -> do
	itext t 1 "initGame = Game (Turn Black) initBoard"
 ]

publicFunctions2 :: Page
publicFunctions2 = [\t -> do
	writeTopTitle t "nextGame"
	text t "", \t -> do
	text t "* 今のターンの石を指定のマスに置く", \t -> do
	text t "* ターンを次の石にする", \t -> do
	text t "* その石にとって盤の状態がパス状態であれば", \t -> do
	text t "* ターンを再度もとの石にもどす", \t -> do
	text t "* それでもなおパス状態であればゲームオーバーとなる", \t -> do
	text t "* このロジックをnextGame関数に書き出そう"
 ]

publicFunctions3 :: Page
publicFunctions3 = [\t -> do
	writeTopTitle t "nextGame"
	text t "", \t -> do
	text t "* Game.hsに以下を書き込もう", \t -> do
	itext t 1 "nextGame :: Game -> (X, Y) -> Maybe Game", \t -> do
	itext t 1 "nextGame g pos = do", \t -> do
	itext t 2 "g' <- flipTurn <$> place g pos", \t -> do
	itext t 2 "if not $ pass g' then return g' else do", \t -> do
	itext t 3 "let g'' = flipTurn g'", \t -> do
	itext t 3 "if not $ pass g'' then return g'' else", \t -> do
	itext t 4 "return $ gameOver g''"
 ]

aboutGameModule :: Page
aboutGameModule = [\t -> do
	writeTopTitle t "エクスポートリスト"
	text t "", \t -> do
	text t "* 必要な関数はそろったので", \t -> do
	text t "* 公開する関数等を宣言しよう", \t -> do
	text t "* モジュール宣言を以下をようにしよう", \t -> do
	itext t 1 "module Game (", \t -> do
	itext t 2 "Game, Turn(..), Disk(..), X(..), Y(..),"
	itext t 2 "initGame, nextGame, turn, disks, placeable"
	itext t 1 ") where"
	text t "* Game型についてできることは", \t -> do
	itext t 1 "- 位置を指定して次の状態ヘ移行する", \t -> do
	itext t 1 "- 今のターンを調べる", \t -> do
	itext t 1 "- 石の位置や石を置けるマスを調べる"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* Board型に足りない情報は現在のターン", \t -> do
	text t "* Game型には現在のターンの情報を追加した", \t -> do
	text t "* パスを考慮したターンの更新を定義した", \t -> do
	text t "* Game型はnextGameでしか変化しないので", \t -> do
	itext t 1 "- ゲームがおかしな状態になることはない", \t -> do
	text t "* 使いかたとしては", \t -> do
	itext t 1 "- ターンを調べる", \t -> do
	itext t 1 "- 石を置く位置を指定してGame値を更新する"
 ]
