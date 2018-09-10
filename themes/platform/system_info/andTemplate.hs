{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.IO.Class
import System.Info

do	liftIO $ putStrLn os
	liftIO $ putStrLn arch
	return []

main :: IO ()
main = putStrLn "Slozsoft"
