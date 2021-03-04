{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Parts

main :: IO ()
main = do
	testDraw "redRect.png" 100 100 redSquare
	testDraw "check.png" 225 225 \cr -> checkPattern cr 225 225
