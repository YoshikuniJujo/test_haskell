module Examples (
	exam1,
	exam2,
	myExam,
	ifExam
) where

import Parser
import Data.Maybe

exam1, exam2, myExam, ifExam :: IO Program
[exam1, exam2, myExam, ifExam] = map (fmap (fromJust . stoneParse) . readFile)
	["example1.stone", "example2.stone", "myExample.stone", "ifExample.stone"]
