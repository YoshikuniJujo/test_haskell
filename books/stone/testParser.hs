import Parser

import Control.Applicative
import Data.Maybe

exam1, exam2, myExam :: IO Program
[exam1, exam2, myExam] = map (fmap (fromJust . stoneParse) . readFile)
	["example1.stone", "example2.stone", "myExample.stone"]

printProgram :: Program -> IO ()
printProgram = putStr . showProgram

showProgram :: Program -> String
showProgram = unlines . map showStatement

showStatement :: Statement -> String
showStatement (If e tb eb) =
	"(if " ++ showPrimary e ++ " " ++ showBlock tb ++
	maybe " " (\b -> " else " ++ showBlock b) eb ++ ")"
showStatement (While e b) =
	"(while " ++ showPrimary e ++ " " ++ showBlock b ++ ")"
showStatement (Expr e) = showPrimary e

showBlock :: Block -> String
showBlock b = "(" ++ unwords (map showStatement b) ++ ")"

showPrimary :: Primary -> String
showPrimary (PNumber n) = show n
showPrimary (PIdentifier i) = i
showPrimary (PString s) = show s
showPrimary (PNegative p) = "-" ++ showPrimary p
showPrimary (POp o) = " " ++ o ++ " "
showPrimary (PInfix l o r) =
	"(" ++ showPrimary l ++ " " ++ o ++ " " ++ showPrimary r ++ ")"
