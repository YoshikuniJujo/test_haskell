module Examples (
	exam1,
	exam2,
	myExam,
	ifExam,
	funExam
) where

import Parser
import Data.Maybe

exam1, exam2, myExam, ifExam, funExam :: IO Program
[exam1, exam2, myExam, ifExam, funExam] =
	map (fmap (fromJust . stoneParse) . readFile) [
		"example1.stone",
		"example2.stone",
		"myExample.stone",
		"ifExample.stone",
		"funExample.stone"]
