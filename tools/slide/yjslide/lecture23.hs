import Lecture

subtitle :: String
subtitle = "第23回 型の階層"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, prelude2,
	life, life2, life3, life4, life5, life6, life7, life8, life9, life10,
	life11, life12, life13, life14,
	life15, life16, life17, life18, life19,
	life20, life21, life22, life23, life24,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellで型の階層を作る方法を見ていこう", \t -> do
	text t "* 前回学んだ存在型とTypeableのcastを使う", \t -> do
	text t "* やや複雑だが非常に巧妙だ", \t -> do
	text t "* オブジェクト指向プログラミングへの回帰", \t -> do
	text t "* オブジェクト指向的に「どうしても」書きたいときに使う", \t -> do
	text t "* upcastやdowncastができる", \t -> do
	text t "* castできない型への変換は明示的にNothingになる", \t -> do
	text t "* 動きとしては以下のようになる", \t -> do
	itext t 1 "- 最上位の型への変換", \t -> do
	itext t 1 "- 最上位の型からの変換"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* オブジェクト指向的な例を挙げるところまではやらない", \t -> do
	text t "* 型の階層を作り、階層間でcastできるところまでを示す", \t -> do
	text t "* 以下の拡張機能が必要", \t -> do
	itext t 1 "- ExistentialQuantification", \t -> do
	itext t 1 "- DeriveDataTypeable"
 ]

life :: Page
life = [\t -> do
	writeTopTitle t "生物の例"
	text t "", \t -> do
	text t "* 生物型を最上位の型とする", \t -> do
	text t "* 生物型の下に動物型と植物型があり", \t -> do
	text t "* 動物型の下に犬型と猫型があり", \t -> do
	text t "* 植物型の下に樹型と草型がある", \t -> do
	text t "* また生物型の直下に細菌型も作ろう"
	text t "", \t -> do
	text t "* そういう例を作っていこう"
 ]

life2 :: Page
life2 = [\t -> do
	writeTopTitle t "SomeLife"
	text t "", \t -> do
	text t "* まずは最上位の型を作成する", \t -> do
	itext t 1 "data SomeLife = forall l . Life l => SomeLife l"
	itext t 2 "deriving Typeable", \t -> do
	text t "* すべての生物型はこの型に変換できる", \t -> do
	text t "* この型から他の生物型への変換が可能", \t -> do
	text t "* Showクラスのインスタンスにしておく", \t -> do
	itext t 0 "instance Show SomeLife where"
	itext t 1 "showsPrec d (SomeLife l) = showParen (d > 10) $"
	itext t 2 "showString \"SomeLife \" . showsPrec 11 l"
 ]

life3 :: Page
life3 = [\t -> do
	writeTopTitle t "Lifeクラス"
	text t "", \t -> do
	text t "* すべての生物はSomeLifeと相互変換できる必要がある", \t -> do
	text t "* その条件を満たすLifeクラスを作成する", \t -> do
	itext t 1 "class (Typeable l, Show l) => Life l where"
	itext t 2 "toLife :: l -> SomeLife"
	itext t 2 "fromLife :: SomeLife -> Maybe l", \t -> do
	text t "* SomeLifeへの変換は必ず成功する", \t -> do
	text t "* SomeLifeからの変換は失敗する可能性がある"
 ]

life4 :: Page
life4 = [\t -> do
	writeTopTitle t "クラス関数のデフォルト値"
	text t "", \t -> do
	text t "* toLifeとfromLifeにはデフォルト値を設定しておく", \t -> do
	itext t 1 "toLife = SomeLife", \t -> do
	itext t 1 "fromLife (SomeLife l) = cast l"
 ]

life5 :: Page
life5 = [\t -> do
	writeTopTitle t "細菌型"
	text t "", \t -> do
	text t "* 細菌型を以下のように定義する", \t -> do
	itext t 1 "data Bacteria = Bacteria deriving (Typeable, Show)", \t -> do
	text t "* Lifeクラスのインスタンスにする", \t -> do
	itext t 1 "instance Life Bacteria", \t -> do
	text t "* toLifeもfromLifeもデフォルト値。where以下は不要", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "> toLife Bacteria"
	itext t 1 "SomeLife Bacteria", \t -> do
	itext t 1 "> fromLife it :: Maybe Bacteria"
	itext t 1 "Just Bacteria"
 ]

life6 :: Page
life6 = [\t -> do
	writeTopTitle t "動物型"
	text t "", \t -> do
	text t "* 動物型を作る", \t -> do
	text t "* この型は他の型をまとめる上位の型となる", \t -> do
	itext t 1 "data Animal = forall a . Life a => Animal a"
	itext t 2 "deriving Typeable", \t -> do
	text t "* Lifeクラスのインスタンスにする", \t -> do
	itext t 1 "instance Life Animal", \t -> do
	text t "* Showクラスのインスタンスにする", \t -> do
	itext t 0 "instance Show Animal where"
	itext t 1 "showPrec d (Animal a) = showParen (d > 10) $"
	itext t 2 "showString \"Animal \" . showsPrec 11 a"
 ]

life7 :: Page
life7 = [\t -> do
	writeTopTitle t "動物型"
	text t "", \t -> do
	text t "* ここがポイント", \t -> do
	text t "* Animalの下位型用のtoLife, fromLifeを定義する", \t -> do
	itext t 1 "animalToLife :: Life a => a -> SomeLife"
	itext t 1 "animalToLife = toLife . Animal", \t -> do
	itext t 1 "animalFromLife :: Life a => SomeLife -> Maybe a"
	itext t 1 "animalFromLife l = do"
	itext t 2 "Animal a <- fromLife l"
	itext t 2 "cast a", \t -> do
	text t "* つまり、SomeLife (Animal ...)のような構造"
 ]

life8 :: Page
life8 = [\t -> do
	writeTopTitle t "犬型"
	text t "", \t -> do
	text t "* 犬型を定義する", \t -> do
	itext t 1 "newtype Dog = Dog String deriving (Typeable, Show)", \t -> do
	text t "* Lifeクラスのインスタンスにする", \t -> do
	itext t 1 "instance Life Dog where"
	itext t 2 "toLife = animalToLife"
	itext t 2 "fromLife = animalFromLife"
 ]

life9 :: Page
life9 = [\t -> do
	writeTopTitle t "犬型"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "> toLife $ Dog \"pochi\""
	itext t 1 "SomeLife (Animal (Dog \"pochi\"))", \t -> do
	itext t 1 "> let dog = it", \t -> do
	itext t 1 "> fromLife dog :: Maybe Dog"
	itext t 1 "Just (Dog \"pochi\")", \t -> do
	itext t 1 "> fromLife dog :: Maybe Animal"
	itext t 1 "Just (Animal (Dog \"pochi\"))", \t -> do
	itext t 1 "> fromLife dog :: Maybe Bacteria"
	itext t 1 "Nothing"
 ]

life10 :: Page
life10 = [\t -> do
	writeTopTitle t "猫型"
	text t "", \t -> do
	text t "* 猫型を定義", \t -> do
	itext t 1 "newtype Cat = Cat String deriving (Typeable, Show)", \t -> do
	itext t 1 "instance Life Cat where"
	itext t 2 "toLife = animalToLife"
	itext t 2 "fromLife = animalFromLife"
 ]

life11 :: Page
life11 = [\t -> do
	writeTopTitle t "植物型"
	text t "", \t -> do
	text t "* 植物型を定義する", \t -> do
	text t "data Plant = forall p . Life p => Plant p"
	itext t 1 "deriving Typeable"
	text t "", \t -> do
	text t "instance Show Plant where"
	itext t 1 "showsPrec d (Plant p) = showParen (d > 10) $"
	itext t 2 "showString \"Plant \" . showsPrec 11 p"
	text t "", \t -> do
	text t "instance Life Plant"
 ]

life12 :: Page
life12 = [\t -> do
	writeTopTitle t "植物型"
	text t "", \t -> do
	text t "* 植物用のtoLife, fromLife", \t -> do
	itext t 1 "plantToLife :: Life p => p -> SomeLife"
	itext t 1 "plantToLife = toLife . Plant"
	itext t 1 "", \t -> do
	itext t 1 "plantFromLife :: Life p => SomeLife -> Maybe p"
	itext t 1 "plantFromLife l = do"
	itext t 2 "Plant p <- fromLife l"
	itext t 2 "cast p"
 ]

life13 :: Page
life13 = [\t -> do
	writeTopTitle t "樹型"
	text t "", \t -> do
	text t "* 樹型", \t -> do
	itext t 1 "newtype Tree = Tree String"
	itext t 2 "deriving (Typeable, Show)"
	itext t 1 "", \t -> do
	itext t 1 "instance Life Tree where"
	itext t 2 "toLife = plantToLife"
	itext t 2 "fromLife = plantFromLife"
 ]

life14 :: Page
life14 = [\t -> do
	writeTopTitle t "草型"
	text t "", \t -> do
	text t "* 草型", \t -> do
	itext t 1 "data Grass = Grass deriving (Typeable, Show)"
	itext t 1 "", \t -> do
	itext t 1 "instance Life Grass where"
	itext t 2 "toLife = plantToLife"
	itext t 2 "fromLife = plantFromLife"
 ]

life15 :: Page
life15 = [\t -> do
	writeTopTitle t "人間型"
	text t "", \t -> do
	text t "* ここで、動物型の下位型となる人間型を定義する", \t -> do
	text t "* 人間型自体も上位型とする", \t -> do
	text t "* 新しい点はない", \t -> do
	itext t 1 "上位型と下位型の性質の両方を持つ"
 ]

life16 :: Page
life16 = [\t -> do
	writeTopTitle t "人間型"
	text t "", \t -> do
	text t "* 人間型を定義する", \t -> do
	itext t 1 "data Human = forall h . Life h => Human h"
	itext t 2 "deriving Typeable", \t -> do
	text t "* Lifeクラスのインスタンスにする", \t -> do
	itext t 1 "instance Life Human where"
	itext t 2 "toLife = animalToLife"
	itext t 2 "fromLife = animalFromLife", \t -> do
	text t "* Showクラスのインスタンスにする", \t -> do
	text t "instance Show Human where"
	itext t 1 "showsPrec d (Human h) = showParen (d > 10) $"
	itext t 2 "showString \"Human\" . showsPrec 11 h"
 ]

life17 :: Page
life17 = [\t -> do
	writeTopTitle t "人間型"
	text t "", \t -> do
	text t "* 人間用のtoLife, fromLifeを定義する", \t -> do
	itext t 1 "humanToLife :: Life h => h -> SomeLife"
	itext t 1 "humanToLife = toLife . Human"
	itext t 1 "", \t -> do
	itext t 1 "humanFromLife :: Life h => SomeLife -> Maybe h"
	itext t 1 "humanFromLife = do"
	itext t 2 "Human h <- fromLife l"
	itext t 2 "cast h"
 ]

life18 :: Page
life18 = [\t -> do
	writeTopTitle t "プログラマ型"
	text t "", \t -> do
	text t "* プログラマ型"
	itext t 1 "newtype Programmer = Programmer String"
	itext t 2 "deriving (Typeable, Show)"
	itext t 1 "", \t -> do
	itext t 1 "instance Life Programmer where"
	itext t 2 "toLife = humanToLife"
	itext t 2 "fromLife = humanFromLife"
 ]

life19 :: Page
life19 = [\t -> do
	writeTopTitle t "作家型"
	text t "", \t -> do
	text t "* 作家型"
	itext t 1 "newtype Author = Author String"
	itext t 2 "deriving (Typeable, Show)"
	itext t 1 "", \t -> do
	itext t 1 "instance Life Author where"
	itext t 2 "toLife = humanToLife"
	itext t 2 "fromLife = humanFromLife"
 ]

life20 :: Page
life20 = [\t -> do
	writeTopTitle t "生物型"
	text t "", \t -> do
	text t "* 生物型自体はまだLifeクラスのインスタンスではない", \t -> do
	text t "* これもLifeクラスのインスタンスにしておくと便利", \t -> do
	itext t 1 "instance Life SomeLife where"
	itext t 2 "toLife = id"
	itext t 2 "fromLife = Just"
 ]

life21 :: Page
life21 = [\t -> do
	writeTopTitle t "castLife"
	text t "", \t -> do
	text t "* Lifeクラスのインスタンス間での型キャスト", \t -> do
	itext t 1 "castLife :: (Life l1, Life l2) => l1 -> Maybe l2"
	itext t 1 "castLife = fromLife . toLife"
 ]

life22 :: Page
life22 = [\t -> do
	writeTopTitle t "castLife", \t -> do
	text t "* 試してみる", \t -> do
	text t "> castLife $ Programmer \"Matz\" :: Maybe Human"
	text t "Just (Human (Programmer \"Matz\"))", \t -> do
	text t "> castLife $ Author \"Souseki\" :: Maybe SomeLife"
	text t "Just (SomeLife (Animal (Human (Author \"Souseki\"))))", \t -> do
	text t "> let Just souseki = it", \t -> do
	text t "> :t souseki"
	text t "souseki :: SomeLife", \t -> do
	text t "> castLife souseki :: Maybe Human"
	text t "Just (Human (Author \"Souseki\"))", \t -> do
	text t "> castLife souseki :: Maybe Plant"
	text t "Nothing"
 ]

life23 :: Page
life23 = [\t -> do
	writeTopTitle t "filterLife"
	text t "", \t -> do
	text t "* 面白い関数が作れる", \t -> do
	itext t 1 "filterLife :: (Life l1, Life l2 => [l1] -> [l2]"
	itext t 1 "filterLife = catMaybe . map castLife", \t -> do
	text t "* この関数は例えば以下のように使う", \t -> do
	itext t 1 "filterLife lives :: [Human]", \t -> do
	itext t 1 "- livesのなかから人間型に属する型のみを抽出する"
 ]

life24 :: Page
life24 = [\t -> do
	writeTopTitle t "withLifeIO"
	text t "", \t -> do
	text t "* 面白い関数が作れる", \t -> do
	itext t 1 "withLifeIO :: (Life l1, Life l2) =>"
	itext t 2 "l1 -> (l2 -> IO ()) -> IO ()"
	itext t 1 "withLifeIO l f = maybe (return ()) f $ castLife l", \t -> do
	text t "* 特定のタイプに属する値にのみ関数を適用する", \t -> do
	itext t 1 "withLifeIO (Author \"souseki\" $ \\(h :: Human) ->"
	itext t 2 "print h", \t -> do
	arrowIText t 2 "Human (Author \"souseki\")", \t -> do
	itext t 1 "withLifeIO Bacteria $ \\(h :: Human) -> print h"
	arrowIText t 2 "何もしない"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 型の階層構造を作ってみた", \t -> do
	text t "* 階層内での型キャストが可能", \t -> do
	text t "* キャストできない場合はNothingが返る", \t -> do
	arrowIText t 1 "安全", \t -> do
	text t "* 特定の範囲の型のみを抽出したり", \t -> do
	text t "* 特定の範囲の型のみにIOアクションを実行したりできる", \t -> do
	text t "* 次回の例外処理にこの仕組みが使われている"
 ]
