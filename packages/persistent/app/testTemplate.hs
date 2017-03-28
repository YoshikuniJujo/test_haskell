import Template (
	Entity(..), tables, persistLowerCase, runDB, selectAll, deleteAll,
	put, newline)
import Database.Esqueleto (
	InnerJoin(..), (^.), (==.),
	insert, select, from, on, unValue, unSqlBackendKey )

import Data.Text (Text)
import qualified Data.Text as Txt (unpack)

tables "migrateAll" [persistLowerCase|
Author
	name Text
	deriving Show
Blog
	author AuthorId
	title Text
	content Text
	deriving Show
|]

main :: IO ()
main = runDB migrateAll $ do
	alice <- insert $ Author "Alice"
	bob <- insert $ Author "Bob"

	_ <- insert $ Blog alice "Alice's first post" "Hello World!"
	_ <- insert $ Blog bob "Bob's first post" "Hello World!"
	_ <- insert $ Blog bob "Bob's second post" "Goodbye World!"

	selectAll >>= put . unlines . map showAuthor
	newline
	selectAll >>= put . unlines . map showBlog

	blogs <- select . from $ \(blog `InnerJoin` author) -> do
		on $ blog ^. BlogAuthor ==. author ^. AuthorId
		return (
			blog	^. BlogId,
			blog	^. BlogTitle,
			author	^. AuthorName )
	newline
	put . unlines $ map (show . three
			(unSqlBackendKey . unBlogKey . unValue)
			unValue
			unValue) blogs
	deleteAll @Blog
	deleteAll @Author
	return ()

showAuthor :: Entity Author -> String
showAuthor = Txt.unpack . authorName . entityVal

showBlog :: Entity Blog -> String
showBlog b_ =
	show (unSqlBackendKey . unAuthorKey $ blogAuthor b) ++ " " ++
	Txt.unpack (blogTitle b) ++ " " ++
	Txt.unpack (blogContent b)
	where b = entityVal b_

three :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
three f g h (x, y, z) = (f x, g y, h z)
