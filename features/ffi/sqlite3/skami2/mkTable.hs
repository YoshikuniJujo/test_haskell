import qualified Database as DB

main :: IO ()
main = newTable

newTable :: IO ()
newTable = do
	conn <- DB.open
	DB.newTable conn
	DB.close conn
