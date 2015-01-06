{-# LANGUAGE ExistentialQuantification #-}

class Sound d where
	sound :: d -> String

data Duck = Duck String deriving Show

instance Sound Duck where
	sound (Duck n) = n ++ " say quack!"

data Cat = Cat String deriving Show

instance Sound Cat where
	sound (Cat n) = n ++ " say myaa!"

data Animal = forall a . Sound a => Animal a

instance Sound Animal where
	sound (Animal a) = sound a

animals :: [Animal]
animals = [Animal (Cat "Mickel"), Animal (Duck "Taro")]

hello :: Sound s => s -> IO ()
hello = print . sound
