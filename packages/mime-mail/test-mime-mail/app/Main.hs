{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.String
import System.Environment
import Network.Mail.Mime

main :: IO ()
main = do
	addr : _ <- getArgs
	(sendmail =<<) . renderMail' $ simpleMail'
		(fromString addr)
		(fromString addr)
		"テストメール(2)"
		"テストメール(2)だよ"
