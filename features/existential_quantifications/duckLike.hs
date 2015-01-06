{-# LANGUAGE ExistentialQuantification #-}

class DuckLike d where
	walk :: d -> String
	quack :: d -> String

type Name = String

data Duck = Duck Name deriving Show

data Goose = Goose Name deriving Show

instance DuckLike Duck where
	walk (Duck n) =
		"Duck " ++ n ++ " walks like a duck."
	quack (Duck n) =
		"Duck " ++ n ++ " quacks like a duck."

instance DuckLike Goose where
	walk (Goose n) =
		"Goose " ++ n ++ " walks like a duck."
	quack (Goose n) =
		"Goose " ++ n ++ " quacks like a duck."

data DuckLikeBox = forall d . DuckLike d => DuckLikeBox d

instance DuckLike DuckLikeBox where
	walk (DuckLikeBox d) = walk d
	quack (DuckLikeBox d) = quack d

duckLikes :: [DuckLikeBox]
duckLikes = [
	DuckLikeBox $ Duck "Mike",
	DuckLikeBox $ Goose "Taro" ]
