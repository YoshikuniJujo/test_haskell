import System.Random

type Name = String
type Age = Int

data Person = Person Name Age deriving Show

nameChars :: [Char]
nameChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ " "

randomNameChar :: RandomGen g => g -> (Char, g)
randomNameChar g = let
	(i, g') = randomR (0, length nameChars - 1) g
	in (nameChars !! i, g')

randomNameL :: RandomGen g => Int -> g -> (String, g)
randomNameL 0 g = ("", g)
randomNameL n g = let
	(c, g') = randomNameChar g
	(cs, g'') = randomNameL (n - 1) g' in
	(c : cs, g'')

randomName :: RandomGen g => g -> (String, g)
randomName g = let (l, g') = randomR (0, 100) g in
	randomNameL l g'
	

instance Random Person where
	random g = let
		(name, g') = randomName g
		(age, g'') = randomR (0, 120) g' in
		(Person name age, g'')
	randomR (Person _ amin, Person _ amax) g = let
		(name, g') = randomName g
		(age, g'') = randomR (amin, amax) g' in
		(Person name age, g'')

data Pol = Pol Double Double deriving Show

instance Random Pol where
	random g = let
		(d, g') = randomR (0, 100) g
		(a, g'') = randomR (0, 2 * pi) g' in
		(Pol d a, g'')
	randomR (Pol dmin amin, Pol dmax amax) g = let
		(d, g') = randomR (dmin, dmax) g
		(a, g'') = randomR (amin, amax) g' in
		(Pol d a, g'')
