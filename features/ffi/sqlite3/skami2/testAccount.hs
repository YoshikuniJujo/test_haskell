{-# LANGUAGE OverloadedStrings #-}

import Account

main :: IO ()
main = do
	conn <- open
	_ <- newAccount conn (UserName "jujo")
		(MailAddress "foo@bar.ne.jp") (Password "yoshikuni")
	ret <- newAccount conn (UserName "jujo2")
		(MailAddress "foo2@bar.ne.jp") (Password "yoshikuni")
	case ret of
		Right u -> activate conn u
		_ -> return ()
	print =<< checkPassword conn (UserName "jujo") (Password "yoshikuni")
	print =<< checkPassword conn (UserName "jujo") (Password "oshikuni")
	print =<< checkPassword conn (UserName "jujo2") (Password "yoshikuni")
	print =<< checkPassword conn (UserName "jujo2") (Password "oshikuni")
	print =<< checkPassword conn (UserName "higa") (Password "oshikuni")
	close conn
