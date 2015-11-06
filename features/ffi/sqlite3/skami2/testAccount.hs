{-# LANGUAGE OverloadedStrings #-}

import Account

main :: IO ()
main = do
	conn <- open
--	newAccount conn (UserName "jujo3")
--		(MailAddress "foo@bar.ne.jp") (Password "yoshikuni")
	print =<< checkPassword conn (UserName "jujo") (Password "yoshikuni")
	print =<< checkPassword conn (UserName "jujo") (Password "oshikuni")
	print =<< checkPassword conn (UserName "jujo2") (Password "yoshikuni")
	print =<< checkPassword conn (UserName "jujo2") (Password "oshikuni")
	close conn
