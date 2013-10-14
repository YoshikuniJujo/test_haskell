module Main where

import Lecture

subtitle :: String
subtitle = "第9回 落ち穂拾い"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, index,
	index' 1, comment,
	index' 2, literate,
	index' 3, offside1, offside2,
	index' 4, whereClause, letin, doLet,
	index' 5, topPatMatch, lambdaPatMatch, doPatMatch,
	index' 6, asPattern,
	index' 7, lazyPattern1, lazyPattern2,
	index' 8
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 初級編最終回", \t -> do
	text t "* 今までの講義で触れられなかった機能について", \t -> do
	text t "* 主な機能については今回で網羅することを目指す", \t -> do
	itext t 1 "- ffiについては中級編で", \t -> do
	itext t 1 "- template haskellについては上級編で"
 ]

contentsList :: [String]
contentsList = [
	"コメント",
	"文芸的プログラミング",
	"オフサイドルール",
	"letやwhereによる変数のローカルな定義",
	"いろいろな場所でのパターンマッチ",
	"アズパターン",
	"遅延パターン",
	"フィールドラベル",
	"パターンガード",
	"ビューパターン"
 ]

index :: Page
index = (\t -> writeTopTitle t "今回の内容") : makeRedList contentsList 0

index' :: Int -> Page
index' i = (: []) $ \t -> writeTopTitle t "今回の内容" >>
	oneshot t (mapM_ ($ t) $ makeRedList contentsList i)

comment :: Page
comment = [\t -> do
	writeTopTitle t "コメント"
	text t "", \t -> do
	text t "* 行コメントとブロックコメントとがある", \t -> do
	text t "* 行コメントは'--'から改行まで", \t -> do
	text t "* ブロックコメントは'{-'から'-}'まで", \t -> do
	text t "* ブロックコメントはネスト可"
 ]

redPage :: (Turtle -> IO ()) -> Turtle -> IO ()
redPage act t = withRed t $ act t

makeRedList :: [String] -> Int -> Page
makeRedList [] _ = []
makeRedList (i : is) 1 = redPage (flip text $ "* " ++ i) : makeRedList is 0
makeRedList (i : is) n = flip text ("* " ++ i) : makeRedList is (n - 1)

literate :: Page
literate = [\t -> do
	writeTopTitle t "文芸的プログラミング"
	text t "", \t -> do
	text t "* 拡張子を.lhsとする", \t -> do
	text t "* 普通に書いたものはコメント", \t -> do
	text t "* '> 'で始まる行がプログラム", \t -> do
	itext t 1 "- '>'の後のスペースは必要", \t -> do
	itext t 1 "- コメントとコードの間には空行が必要", \t -> do
	text t "* '\\begin{code}'から'\\end{code}'までの間もコードとなる"
 ]

offside1 :: Page
offside1 = [\t -> do
	writeTopTitle t "オフサイドルール"
	text t "", \t -> do
	text t "* Haskellでは'{', '}', ';'が省略できる", \t -> do
	text t "* '{'の省略があった場合にオフサイドルールが始まる", \t -> do
	text t "* '{'の省略の次の単語の先頭がインデントの基準となる", \t -> do
	text t "* 基準よりも深いインデントであれば何もしない", \t -> do
	text t "* 基準と同じインデントであれば直前に';'を挿入する", \t -> do
	text t "* 基準より浅いインデントであれば直前に'}'を挿入する", \t -> do
	text t "* パースに失敗した際にはそこに'}'を入れて再度パース", \t -> do
	dvArrowShort t
	semititle t "直観的に書けるようになっている"
 ]

offside2 :: Page
offside2 = [\t -> do
	writeTopTitle t "オフサイドルール"
	text t "", \t -> do
	text t "do someIO"
	itext t 0.5 "otherIO"
	itext t 0.5 "anotherIO", \t -> do
	dvArrowShort t
	text t "do { someIO"
	itext t 0.8 ";otherIO"
	itext t 0.8 ";otherIO"
	itext t 0.8 ";anotherIO"
	text t "}"
 ]

whereClause :: Page
whereClause = [\t -> do
	writeTopTitle t "where節"
	text t "", \t -> do
	text t "* 関数定義にはwhere節がつけられる", \t -> do
	text t "* ガードにまたがるローカルな変数", \t -> do
	text t "* スコープ以外はトップでの定義と同じ"
	text t "", \t -> do
	text t "例:"
	text t "fun x"
	itext t 0.5 "| x < 10 = x2"
	itext t 0.5 "| otherwise = x2 + x2"
	itext t 0.5 "where x2 = x * x"
 ]

letin :: Page
letin = [\t -> do
	writeTopTitle t "let ... in ..."
	text t "", \t -> do
	text t "* let [定義] in [表現]で定義を[表現]中で使える"
	text t "", \t -> do
	text t "例:"
	text t "let x = 3 + 9 in x + x => 24"
 ]

doLet :: Page
doLet = [\t -> do
	writeTopTitle t "do内でのlet"
	text t "", \t -> do
	text t "* do構文のなかでもletは使える", \t -> do
	text t "* この場合スコープはlet以降のdo内部"
	text t "", \t -> do
	text t "例:"
	text t "do someIO"
	itext t 0.5 "let x = 8"
	itext t 0.5 "otherIO x"
 ]

topPatMatch :: Page
topPatMatch = [\t -> do
	writeTopTitle t "パターン束縛", \t -> do
	text t "* モジュールトップでもパターンマッチは使える", \t -> do
	itext t 1 "- let構文、where節でも同様"
	text t "", \t -> do
	text t "例:"
	text t "x, y :: Int"
	text t "(x, y) = (3, 8)"
	text t "", \t -> do
	text t "z, w :: Double"
	text t "[z, w] = [9, 3.8]"
	text t "", \t -> do
	text t "h :: Char; ello :: String"
	text t "h : ello = \"hello\""
 ]

lambdaPatMatch :: Page
lambdaPatMatch = [\t -> do
	writeTopTitle t "関数リテラルでのパターンマッチ"
	text t "", \t -> do
	text t "* 関数リテラルの引数部分でもパターンマッチは可能"
	text t "", \t -> do
	text t "例:"
	text t "\\(_ : xs) -> xs"
 ]

doPatMatch :: Page
doPatMatch = [\t -> do
	writeTopTitle t "do構文内でのパターンマッチ"
	text t "", \t -> do
	text t "* [パターン] <- [表現]という形"
	text t "", \t -> do
	text t "例:"
	text t "do someIO"
	itext t 0.5 "x : xs <- retListIO"
	itext t 0.5 "otherIO x"
 ]

asPattern :: Page
asPattern = [\t -> do
	writeTopTitle t "アズパターン"
	text t "", \t -> do
	text t "* 例えばリストのheadとtailだけでなく全体も使いたい", \t -> do
	itext t 1 "dupHead (x : xs) = x : x : xs", \t -> do
	text t "* 全体をx : xsのように分けたうえで再度結合している", \t -> do
	arrowIText t 1 "美しくない", \t -> do
	text t "* 全体を変数に束縛したうえでパターンマッチすれば良い", \t -> do
	arrowIText t 1 "できるよ", \t -> do
	dvArrowShort t
	text t "dupHead xs@(x : _) = x : xs"
 ]

lazyPattern1 :: Page
lazyPattern1 = [\t -> do
	writeTopTitle t "遅延パターン"
	text t "", \t -> do
	text t "* 以下の場合、パターンマッチをすぐに行う", \t -> do
	itext t 1 "- case構文や関数の仮引数部、かつ", \t -> do
	itext t 1 "- 単純な仮引数やワイルドパターンでない", \t -> do
	arrowIText t 1 "そうしないとどのパターンにマッチするかわからない", \t -> do
	text t "* パターン照合を遅延させる", \t -> do
	itext t 1 "- 値の中身を見ずにパターンマッチを成功とする", \t -> do
	arrowIText t 1 "不可反駁パターンとも"
	text t "", \t -> do
	semititle t "パターンの前に'~'をつける"
 ]

lazyPattern2 :: Page
lazyPattern2 = [\t -> do
	writeTopTitle t "遅延パターン"
	text t "たとえば、2のべき乗のリストを作る"
	text t "", \t -> do
	text t "* iterate (* 2) 1とすれば良い", \t -> do
	text t "* もっと違うやりかたでやろう!", \t -> do
	itext t 1 "consMap f y0 (x : xs) = y0 : consMap f (f x) xs", \t -> do
	itext t 1 "expo2 = consMap (* 2) 1 expo2", \t -> do
	text t "* expo2の最初の値を得るときexpo2の(x : xs)でのマッチ", \t -> do
	arrowIText t 1 "無限ループ", \t -> do
	arrowIText t 1 "パターンマッチを遅らせてやれば良い", \t -> do
	itext t 1 "consMap' f y0 ~(x : xs) = y0 : consMap f (f x) xs", \t -> do
	itext t 1 "expo2' = consMap' (* 2) 1 expo2'"
 ]
