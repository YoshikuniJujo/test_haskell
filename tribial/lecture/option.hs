data Option a b = Single a | Option a b deriving Show

human :: Option String Int -> String
human (Single n) = n
human (Option n a) = n ++ " (" ++ show a ++ ")"
