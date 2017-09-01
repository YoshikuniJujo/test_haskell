{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Posix.User

main :: IO ()
main = do
	getRealUserID >>= print
	getEffectiveUserID >>= print
