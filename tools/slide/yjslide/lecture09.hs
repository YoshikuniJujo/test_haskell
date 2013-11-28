module Main where

import Lecture

subtitle :: String
subtitle = "第9回 落ち穂拾い"

main :: IO ()
main = runLecture pages

memo :: Page
memo = [\t -> do
	writeTopTitle t "TODO"
	text t "", \t -> do
	semititle t "infix, infixl, infixrについて追加すること!", \t -> do
	semititle t "データ構築演算子、型構築演算子について追加!"
 ]

pages :: [Page]
pages = [
	titlePage, memo, prelude, index,
	index' 1, comment,
	index' 2, literate,
	index' 3, offside1, offside2,
	index' 4, whereClause, letin, doLet,
	index' 5, typeSynonym,
	index' 6, aboutNewtype,
	index' 7, topPatMatch, lambdaPatMatch, doPatMatch,
	index' 8, asPattern,
	index' 9, lazyPattern1, lazyPattern2,
	index' 10, fieldLabel1, fieldLabel2,
	index' 11, listComprehension,
	index' 12, patternGuard,
	index' 13, viewPattern,
	index' 0,
	summary
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
	"タイプシノニム",
	"newtype",
	"いろいろな場所でのパターンマッチ",
	"アズパターン",
	"遅延パターン",
	"フィールドラベル",
	"リスト内包表記",
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

typeSynonym :: Page
typeSynonym = [\t -> do
	writeTopTitle t "タイプシノニム"
	text t "", \t -> do
	text t "* タイプには別名をつけることができる", \t -> do
	text t "* type [別名] = [型]という形"
	text t "* 別名なので元の型と同じものである"
	text t "", \t -> do
	text t "例:"
	text t "type Name = String"
	text t "type NameList = [Name]"
 ]

aboutNewtype :: Page
aboutNewtype = [\t -> do
	writeTopTitle t "newtype"
	text t "", \t -> do
	text t "* data宣言で新しい型を作るとき", \t -> do
	itext t 1 "- 型構築子がひとつ", \t -> do
	itext t 1 "- 型構築子が取る型もひとつの場合", \t -> do
	itext t 1 "- つまり、既存の型をwrapする場合", \t -> do
	arrowIText t 1 "newtypeが使える", \t -> do
	text t "* 意味的にはdata宣言とほぼ同じ", \t -> do
	text t "* dataとの違いはオーバーヘッドが生じないこと", \t -> do
	text t "* typeとは異なり元の型とは区別される"
	text t "", \t -> do
	text t "例:"
	text t "newtype Name = Name String"
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

fieldLabel1 :: Page
fieldLabel1 = [\t -> do
	writeTopTitle t "フィールドラベル", \t -> do
	text t "* data宣言でフィールドにラベルを付ける記法がある", \t -> do
	itext t 1 "data Human = Human String String Int", \t -> do
	dvArrowShort t
	itext t 1 "data Human = Human {"
	itext t 2 "firstName :: String,"
	itext t 2 "secondName :: String,"
	itext t 2 "age :: Int }"
	text t "フィールドラベルはいろいろな場面で使える", \t -> do
	itext t 1 "- 新たに値を作る", \t -> do
	itext t 1 "- フィールドの値をセットする", \t -> do
	itext t 1 "- フィールドの値を得る", \t -> do
	itext t 1 "- パターンマッチ"
 ]

fieldLabel2 :: Page
fieldLabel2 = [\t -> do
	writeTopTitle t "フィールドラベル", \t -> do
	text t "* 新たに値を作る"
	itext t 1 "taro = Human {"
	itext t 2 "firstName = \"太郎\""
	itext t 2 "secondName =\"山田\""
	itext t 2 "age = 56 }", \t -> do
	text t "* フィールドの値をセットする"
	itext t 1 "taro' = taro { age = 83 }", \t -> do
	text t "* フィールドの値を得る"
	itext t 1 "secondName taro => \"山田\""
	text t "* パターンマッチ"
	itext t 1 "name Human { firstName = f, secondName = s } ="
	itext t 2 "s ++ f"
 ]

listComprehension :: Page
listComprehension = [\t -> do
	writeTopTitle t "リスト内包表記"
	text t "", \t -> do
	text t "* 本質的にはfilterとconcatMapに脱糖される糖衣構文", \t -> do
	itext t 1 "例: [x + y | x <- [5, 4], y <- [1, 2 3]]"
	itext t 2 "=> [6, 7, 8, 5, 6, 7]", \t -> do
	itext t 1 "例2:[x + y | x <- [5, 4], y <- [1, 2, 3],"
	itext t 2 "x * y < 12] => [6, 7, 5, 6]", \t -> do
	text t "* '|'の左側が最終的な結果", \t -> do
	text t "* '|'の右側は値の取り出しまたは条件を','でつないだもの", \t -> do
	text t "* 値の取り出しにはパターンマッチが使える", \t -> do
	itext t 1 "- マッチしなければ最終的な結果にいれない"
 ]

patternGuard :: Page
patternGuard = [\t -> do
	writeTopTitle t "パターンガード"
	text t "", \t -> do
	text t "* Haskell 2010で新たに採用された便利構文", \t -> do
	text t "* 関数のガードのところに対する自然な拡張", \t -> do
	text t "* ガード部にリスト内包表記の'|'の右側が使える"
	text t "", \t -> do
	text t "例:"
	text t "addLookup env var1 var2"
	itext t 1 "| Just val1 <- lookup env var1,"
	itext t 1 ", Just val2 <- lookup env var2 ="
	itext t 2 "val1 + val2"
	text t "addLookup _ _ _ = 0"
 ]

viewPattern :: Page
viewPattern = [\t -> do
	writeTopTitle t "ビューパターン"
	text t "", \t -> do
	text t "* Haskell 2010には取り入れられていない拡張機能", \t -> do
	text t "* コードの先頭に{-# LANGUAGE ViewPatterns #-}が必要", \t -> do
	text t "* パターンガードとすこし似ている", \t -> do
	text t "* パターンマッチの前に関数適用することができる"
	text t "", \t -> do
	text t "addLookup env"
	itext t 1 "((`lookup` env) -> Just val1)"
	itext t 1 "((`lookup` env) -> Just val2) = val1 + val2"
	text t "addLookup _ _ _ = 0"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 第8回までの講義で触れられなかった機能", \t -> do
	text t "* 多くは表記に関する話", \t -> do
	text t "* これらの機能を使いこなせばより簡潔にきれいに書ける", \t -> do
	text t "* 今回で初級編は終わり", \t -> do
	text t "* 次回からは中級編", \t -> do
	text t "* 中級編の内容", \t -> do
	itext t 1 "- テスト、プロファイリング", \t -> do
	itext t 1 "- いろいろなモナド", \t -> do
	itext t 1 "- 正格評価", \t -> do
	itext t 1 "- 実行効率の改善について"
 ]
