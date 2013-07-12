module FileSystem (
	Name, Data, FSItem(..), FSZipper,
	fsUp, fsTo, fsRename, fsNewFile,
	fsGetDirContents, fsGetContents
) where

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving Show

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving Show
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> Maybe FSZipper
fsUp (item, FSCrumb name ls rs:bs) =
	Just (Folder name (ls ++ [item] ++ rs), bs)
fsUp _ = Nothing

fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo name (Folder folderName items, bs) =
	case break (nameIs name) items of
		(ls, item : rs) -> Just (item, FSCrumb folderName ls rs : bs)
		_ -> Nothing

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

getName :: FSItem -> Name
getName (Folder fn _) = fn
getName (File fn _) = fn

(-:) :: a -> (a -> b) -> b
x -: f = f x

fsRename :: Name -> FSZipper -> Maybe FSZipper
fsRename newName (Folder name items, bs) = Just (Folder newName items, bs)
fsRename newName (File name dat, bs) = Just (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> Maybe FSZipper
fsNewFile item (Folder folderName items, bs) =
	Just (Folder folderName (item : items), bs)
fsNewFile _ _ = Nothing

fsGetDirContents :: FSZipper -> Maybe [String]
fsGetDirContents (Folder _ items, _) = Just $ map getName items
fsGetDirContents _ = Nothing

fsGetContents :: FSZipper -> Maybe String
fsGetContents (File _ dat, _) = Just dat
fsGetContents _ = Nothing
