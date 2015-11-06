import Account

main :: IO ()
main = do
	conn <- open
	newTable conn
	close conn
