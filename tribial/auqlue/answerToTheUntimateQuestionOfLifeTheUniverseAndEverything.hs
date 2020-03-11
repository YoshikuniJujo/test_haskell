{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

answerToTheUltimateQuestionOfLifeTheUniverseAndEverything :: Integer
answerToTheUltimateQuestionOfLifeTheUniverseAndEverything =
	(- 80538738812075974) ^ (3 :: Int) +
	80435758145817515 ^ (3 :: Int) +
	12602123297335631 ^ (3 :: Int)

main :: IO ()
main = do
	putStr $ "Answer to " ++
		"the Ultimate Question of Life, the Universe, and Everything: "
	print answerToTheUltimateQuestionOfLifeTheUniverseAndEverything
