import Template
import Database.Esqueleto

import Data.Text (Text)

tables "migrateAll" [persistLowerCase|
Author
	name Text
	deriving Show
|]

main :: IO ()
main = runDB migrateAll $ do
	alice <- insert $ Author "Alice"
	selectAll @Author >>= put . unlines . map show
	deleteAll @Author
