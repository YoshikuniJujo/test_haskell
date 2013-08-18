module Examples (
	exam1,
	exam2,
	myExam,
	ifExam,
	funExam
) where

import Parser
import Data.Maybe

exam1, exam2, myExam, ifExam, funExam, clExam, classExam :: IO Program
[exam1, exam2, myExam, ifExam, funExam, clExam, classExam] =
	map (fmap (fromJust . stoneParse) . readFile) [
		"example1.stone",
		"example2.stone",
		"myExample.stone",
		"ifExample.stone",
		"funExample.stone",
		"clExample.stone",
		"classExample.stone"]
