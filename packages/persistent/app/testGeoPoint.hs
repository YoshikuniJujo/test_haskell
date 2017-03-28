import Template
import Database.Esqueleto
import Data.Text (Text)

import Geo

tables "migrateAll" [persistLowerCase|
Author
	name Text
	deriving Show
Address
	geo Geo
	deriving Show
|]

main :: IO ()
main = runDB migrateAll $ do
	alice <- insert $ Author "Alice"
	selectAll @Author >>= put . unlines . map show
	deleteAll @Author
	_ <- insert . Address $ toPoint 44 44
	selectAll @Address >>= put . unlines . map show
	deleteAll @Address
