{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

main :: IO ()
main = do
	putStrLn "Slozsoft"
	cxx_check_im_wchar

foreign import ccall "check_im_wchar" cxx_check_im_wchar :: IO ()
