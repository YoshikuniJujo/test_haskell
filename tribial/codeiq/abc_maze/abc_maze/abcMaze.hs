import Control.Applicative
import Control.Arrow (first, second)
import Control.Monad
import Data.Maybe

main :: IO ()
main = interact $ (++ "possible\n") . ifte "" "im" . check1 [] . start
	where ifte x _ Nothing = x; ifte _ y _ = y

check :: [(Int, Int)] -> [Maze] -> Maybe [(Int, Int)]
check h [] = Just h
check h (m : ms) = check1 h m >>= (`check` ms)

check1 :: [(Int, Int)] -> Maze -> Maybe [(Int, Int)]
check1 h m | goal m = Nothing
check1 h m@(Maze p _ _) =
	check (p : h) $ (\(Maze q _ _) -> q `notElem` h) `filter` step m

data Maze = Maze (Int, Int) ABC Map deriving Show

start :: String -> Maze
start s = Maze (0, 0) (char m) m where m = readMap s

goal :: Maze -> Bool
goal (Maze _ _ m) = bottomRight m

step :: Maze -> [Maze]
step = flip mapMaybe [goUp, goDown, goLeft, goRight] . flip ($)

goUp, goDown, goLeft, goRight :: Maze -> Maybe Maze
goUp = go up $ first pred
goDown = go down $ first succ
goLeft = go left $ second pred
goRight = go right $ second succ

go :: (Map -> Maybe Map) -> ((Int, Int) -> (Int, Int)) -> Maze -> Maybe Maze
go d cp (Maze p c m) = do
	m' <- d m
	let c' = char m'
	guard $ c' == next c
	return $ Maze (cp p) c' m'

data ABC = A | B | C deriving (Show, Eq)

readABC :: Char -> ABC
readABC 'A' = A
readABC 'B' = B
readABC 'C' = C
readABC _ = error "readABC: error"

next :: ABC -> ABC
next A = B
next B = C
next C = A

data Map = Map [([ABC], [ABC])] [([ABC], [ABC])] deriving Show

readMap :: String -> Map
readMap = uncurry Map . ((,) []) . map (((,) []) . (map readABC)) . lines

bottomRight :: Map -> Bool
bottomRight (Map us [d]) = all ((== 1) . length . snd) $ d : us
bottomRight _ = False

char :: Map -> ABC
char (Map _ ((_, c : _) : _)) = c
char _ = error "char: bad Map"

up, down, left, right :: Map -> Maybe Map
up (Map us ds) = uncurry Map <$> backward us ds
down (Map us ds) = uncurry Map <$> foreward us ds
left (Map us ds) =
	Map <$> mapM (uncurry backward) us <*> mapM (uncurry backward) ds
right (Map us ds) =
	Map <$> mapM (uncurry foreward) us <*> mapM (uncurry foreward) ds

foreward, backward :: [a] -> [a] -> Maybe ([a], [a])
foreward _ [] = Nothing
foreward _ [_] = Nothing
foreward bs (f : fs) = Just (f : bs, fs)
backward [] _ = Nothing
backward (b : bs) fs = Just (bs, b : fs)
