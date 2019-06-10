{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import TryFetchInstruction

main :: IO ()
main = do
	print $ tryFetchInstructionTryRun !! 50
	print $ tryFetchInstructionTryRun !! 100
	print $ tryFetchInstructionTryRun !! 150
	print $ tryFetchInstructionTryRun !! 200
