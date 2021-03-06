{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import System.IO
import Text.Nowdoc

import ParserCombinator

main :: IO ()
main = doWhile_ $ do
	putStr "> "
	hFlush stdout
	l <- getLine
	case l of
		(':' : cmd) -> runCmd cmd
		_ -> do	print $ evaluate l
			return True

runCmd :: String -> IO Bool
runCmd "q" = return False
runCmd "quit" = return False
runCmd "lena" = putStrLn [nowdoc|
==\...........\$$=\\\\\\\\\\\\\\\\\... ......   .\==$$$$$$$$$$$$=\..\\==\       
==\............$$$==\\\\\\\\\\\\...    ..... ..\==$$$$$$$$$$=\\\\\\===\        .
==\............\$$==\\\\\\\\\....  .      ...\=$$$=====$$$$\...\\===.         .\
==\..........\\.\$$\\\\\\\\..   .  ..     ..\=$$======$$$$$=\...\=\          .\\
==\.........\\\.\$=\\\\\\..         ..  .\====$======$$$$$$$=.  .=.          \\=
==\.........\\\\\\=\\\\.           . ..\==$==$======$$$$$$$$$\   \.         \\==
==\.........\\\\\..\\\.  .           .\===$$=\\\=====$$$$$$$$\   \\        .\\==
==\.........\\\\\..\...  .         .\==$===\\\\..\\===$$$==\..   .\       .\\===
==\.........====\.   ....         .\=$$$\.    \. .\\==$$\. .     .=.      \\====
==\.........===.      ...        .\=$$=\\....\=$\\\\\=$=...\.    .=..    .\=====
==\....\\\\\\.  ...  ..\\       .==$=\\\==\\\====\\\\=$$=\\\..    =\     \\=====
==\...\\\\\.  ....   .....     .=$=..\\\==========\\\=$$==\\\.    \\    .\======
==\...\\\......\.        \.   .=$\  .\\\=========\\\\=$$===\\.    \\   .\=======
==\..\\\.... ..\.     ...\.  .\=\   .\\\\========\\\\\$$===\\.    .=.  \\=======
==\...\\... ....\.    ...  .\==.   ..\\\\\======\\\\\\=$==\\.     .=\ .\========
==\..... .......\\  .  ....\=\    ...\\\\\=======\\\\\===\\.       \\ \=========
==\...\\ ..   ...\\.. .\==\\.     ...\\\\\\\========$$$==\.    .   \\\\=========
==\...\. .   ........\==\\.        ..\\\\\\\\\\\\\\\==\\\\     .   .\\==========
==\...\..    .\.....\\==\.         ...\\\\\\\\\\\\\===\\.      .   .============
==\..\. .    \\\..\.\\=\.            ...\\\\\\\\\\\\\=\.       ..  .===========\
==\......    \..\\.\\\\                 ..\\\\\=======.       .... .===========\
==\...       \...\\\\\.                  ..\\\\=======.       .... .=\\\\\\\\=\\
===\..     . .\..\..\.\\.              .\\\\\\\\=====$$==\.   .... .=\\\\\\\\\\\
==\\.   .. .. ......\\\...             .\\\\\\\\\\===$$$$$$$\. . . .=\\\\\\\\...
|] >> return True
runCmd "panda" = putStrLn [nowdoc|
......\\\\\\\................  .......          ................................
......\\\\\\\\\.............   .......           ....\..........................
.......\\\\\\\\\\\\\\\........\\\\\\\\.           \=$==\\\......................
........\\\\\\\\\\\\\\..\\=$$$$$$$$$$=\           .=$$$$$=\\\\..\...............
........\\\\\\\\\\\..\=$$$$$$$$$$$$$$$==\.        .=$$$$$$$=\\\===============\=
.........\\\\\\\\..\=$$$$$$$$$$$$$$$$$$$$$=\\.. ..\$$$$$$$$=....\=$$$$$$$$$$$$$$
..........\\\\\\\\\=$$$$$$$$$$$$$$$$$$$$$$$$$===\\=$$$$$$==\..   .\=$$$$$$$$$$$$
..........\\\\\\\=$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$=\.       .\=$$$$$$$$$$
..\\\\\\\\\======$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$=\.        .\=$$$$$$$$$
\\\\..    .\\\==$==$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$==\.         \=$$$$$$$$
.\\.          .=========$$$$$$$$$$$$====$$$$$$$$$$$$$$$$$==.           \=$$$$$$$
....           \=============$$$$$$=.  ..\==$$$$$$$$$$$==\.            .\=$$$$$$
               \========\\==========       .\===$$$$$$$=\               .\=$$$$$
....           =======\.  .\========\.       .=====$$$$=\                .\=$$$$
.\\.          .======\\     \=======$$=\.    .\=========\.               .\==$$$
....          \=====\\.     \======$$$$$=\...\========\\.                .\====$
...          .\==\\\\\     \=$$$$=$$$$$$$$$======\\\\..                  .\\====
..           .\\\\\\\.     \=======$$$$$$$$$$===\\\..                    .\\====
...          .\\\\\\\\.....\=====\\\\\==$$$$$==\\..                      ..\\===
...           ..\....\\\\\\\\=\....    \=======\..                        ..\\==
...                    ....\\\\.     ..\\=\\\\\..                         ...\\=
...                         ..\\\......\\\\...                              .\\\
..                              .......\..                                 ...\\
.                                   ...                               ......\\\\
|] >> return True
runCmd _ = putStrLn "no such command" >> return True

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = do
	r <- act
	when r (doWhile_ act)

evaluate :: String -> Maybe Integer
evaluate l = eval <$> prsPTree `parse` l

data PTree
	= Integer Integer
	| Apply Primitive PTree PTree
	deriving Show

data Primitive = Add | Sub | Mul | Div deriving Show

prsPTree, prsInt, prsApply :: Parse PTree
prsPTree = prsInt <|> prsApply

prsInt = (Integer . read <$>) . some $ check isDigit

prsApply = char '(' *> pure Apply
	<*> prsPrim <* some (check isSpace)
	<*> prsPTree <* some (check isSpace)
	<*> prsPTree <* char ')'

prsPrim :: Parse Primitive
prsPrim =
	Add <$ char '+' <|>
	Sub <$ char '-' <|>
	Mul <$ char '*' <|>
	Div <$ char '/'

eval :: PTree -> Integer
eval (Integer n) = n
eval (Apply o t1 t2) = getOp o (eval t1) (eval t2)

getOp :: Primitive -> Integer -> Integer -> Integer
getOp Add = (+)
getOp Sub = (-)
getOp Mul = (*)
getOp Div = div
