{-# LANGUAGE OverloadedStrings #-}

import Account

main :: IO ()
main = do
	conn <- open
	newAccount conn (UserName "jujo3")
		(MailAddress "foo@bar.ne.jp") (Password "yoshikuni")
	close conn
