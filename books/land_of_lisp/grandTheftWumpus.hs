import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Data.Function
import System.Random
import System.Cmd

data NodeState
	= Wumpus | Blood
	| GlowWorm | Lights
	| Siren
	| Player | Unknown
	deriving (Show, Eq, Ord)

showState :: NodeState -> String
showState Player = "*"
showState Unknown = "?"
showState st = show st

data Node = Node Int deriving (Show, Eq, Ord)

instance Enum Node where
	toEnum = Node
	fromEnum (Node e) = e

nodeNum, edgeNum, wormNum, copOdds :: Int
nodeNum = 30
edgeNum = 45
wormNum = 3
copOdds = 15

randomNode :: IO Node
randomNode = Node <$> randomRIO (1, nodeNum)

randomNodes :: IO [Node]
randomNodes = map Node . randomRs (1, nodeNum) <$> newStdGen

edgePair :: Node -> Node -> [(Node, Node)]
edgePair a b
	| a /= b = [(a, b), (b, a)]
	| otherwise = []

zipWithPair :: (a -> a -> b) -> [a] -> [b]
zipWithPair op (x : y : ys) = x `op` y : zipWithPair op ys
zipWithPair _ _ = []

edgePairs :: Int -> [Node] -> [(Node, Node)]
edgePairs n nds = concat . zipWithPair edgePair $ take (2 * n) nds

makeEdgeList :: IO [(Node, Node)]
makeEdgeList = edgePairs edgeNum <$> randomNodes

directEdges :: Node -> [(Node, Node)] -> [Node]
directEdges node = map snd . filter ((node ==) . fst)

getConnected :: Node -> [(Node, Node)] -> [Node]
getConnected = getConnectedGen [] . (: [])

getConnectedGen :: [Node] -> [Node] -> [(Node, Node)] -> [Node]
getConnectedGen pnds [] _ = pnds
getConnectedGen pnds nds eds = getConnectedGen
	(pnds ++ nds)
	(nub . sort . filter (`notElem` pnds)
		$ concatMap (flip directEdges eds) nds)
	eds

findIslands :: [Node] -> [(Node, Node)] -> [Node]
findIslands [] _ = []
findIslands (nd : nds) eds = nd : findIslands (nds \\ getConnected nd eds) eds

connectWithBridges :: [Node] -> [(Node, Node)]
connectWithBridges (nd1 : nds@(nd2 : _)) =
	edgePair nd1 nd2 ++ connectWithBridges nds
connectWithBridges _ = []

connectAllIslands :: [Node] -> [(Node, Node)] -> [(Node, Node)]
connectAllIslands nds eds = connectWithBridges (findIslands nds eds) ++ eds

makeCityEdges :: IO [(Node, [(Node, Bool)])]
makeCityEdges = do
	edgeList <- makeEdgeList
	cs <- map (== 0) . randomRs (0, copOdds - 1) <$> newStdGen
	let cops = map snd . filter fst $ zip cs edgeList
	return . flip addCops cops . edgesToAlist
		$ connectAllIslands [Node 1 .. Node nodeNum] edgeList

edgesToAlist :: [(Node, Node)] -> [(Node, [Node])]
edgesToAlist =
	map (\e -> (fst $ head e, map snd e)) . groupBy (on (==) fst) . nub . sort

addCops :: [(Node, [Node])] -> [(Node, Node)] -> [(Node, [(Node, Bool)])]
addCops ea ewc = map (flip addCops1 ewc) ea

addCops1 :: (Node, [Node]) -> [(Node, Node)] -> (Node, [(Node, Bool)])
addCops1 (n0, ns) ewc = (n0, map (\n -> (n, n `elem` nc)) ns)
	where
	nc = map snd $ filter ((== n0) . fst) ewc

type EdgeAlist = [(Node, [(Node, Bool)])]

neighbors :: Node -> EdgeAlist -> [Node]
neighbors = ((map fst . fromJust) .) . lookup

withinOne :: Node -> Node -> EdgeAlist -> Bool
withinOne a b ea = b `elem` neighbors a ea

withinTwo :: Node -> Node -> EdgeAlist -> Bool
withinTwo a b ea = withinOne a b ea ||
	not (null . filter (flip (withinOne b) ea) $ neighbors a ea)

makeCityNodes :: EdgeAlist -> IO [(Node, [NodeState])]
makeCityNodes ea = do
	w <- randomNode
	gws <- replicateM wormNum randomNode
	return $ map
		(\n -> (n, getNodeState n w gws ea))
		[Node 1 .. Node nodeNum]

getNodeState :: Node -> Node -> [Node] -> EdgeAlist -> [NodeState]
getNodeState n w gws ea = wst ++ gwst ++ srst
	where
	wst = case (w == n, withinTwo n w ea) of
		(True, _) -> [Wumpus]
		(_, True) -> [Blood]
		_ -> []
	gwst = case (n `elem` gws, or $ map (flip (withinOne n) ea) gws) of
		(True, _) -> [GlowWorm]
		(_, True) -> [Lights]
		_ -> []
	srst = if or . map snd . fromJust $ lookup n ea then [Siren] else []

newGame :: IO (EdgeAlist, [(Node, [NodeState])], Node, [Node])
newGame = do
	ea <- makeCityEdges
	ns <- makeCityNodes ea
	pp <- findEmptyNode ns
	drawCity ns ea
	drawKnownCity pp [pp] ns ea
	return (ea, ns, pp, [pp])

findEmptyNode :: [(Node, [NodeState])] -> IO Node
findEmptyNode ndst = do
	nds <- randomNode
	case lookup nds ndst of
		Just [] -> return nds
		_ -> findEmptyNode ndst

drawCity :: [(Node, [NodeState])] -> EdgeAlist -> IO ()
drawCity ns ea = ugraphPng "city" ns ea

ugraphPng :: FilePath -> [(Node, [NodeState])] -> EdgeAlist -> IO ()
ugraphPng fp nds eds = dotPng fp $ ugraphDot nds eds

dotPng :: FilePath -> String -> IO ()
dotPng fp d = do
	writeFile fp d
	_ <- system $ "dot -Tpng -O " ++ fp
	return ()

ugraphDot :: [(Node, [NodeState])] -> EdgeAlist -> String
ugraphDot nds eds = "graph{\n"
	++ nodesDot nds
	++ uedgesDot eds
	++ "}\n"

nodesDot1 :: (Node, [NodeState]) -> String
nodesDot1 (Node nd, ndst) = show nd
	++ "[label=\"" ++ show nd
	++ (if null ndst then "" else " " ++ unwords (map showState ndst))
	++ "\"];"

nodesDot :: [(Node, [NodeState])] -> String
nodesDot = unlines . map nodesDot1

uedgesDot1 :: (Node, [(Node, Bool)]) -> String
uedgesDot1 (Node n0, ncs) = unlines
	. map (((show n0 ++ " -- ") ++) . (++ ";") . mkDt)
	$ filter ((> n0) . (\(Node n) -> n) . fst) ncs
	where
	mkDt (Node n, c) = show n ++ if c then "[label=\"(COPS)\"]" else ""

uedgesDot :: EdgeAlist -> String
uedgesDot = concatMap uedgesDot1

knownNeighbors :: [Node] -> EdgeAlist -> [Node]
knownNeighbors vns ea = nub . sort $ vns ++ concatMap (\n -> neighbors n ea) vns

addStates1 :: Node -> [Node] -> [(Node, [NodeState])] -> Node -> (Node, [NodeState])
addStates1 pp vnds ndsts nd
	| nd `notElem` vnds = (nd, [Unknown])
	| nd == pp = (nd, fromJust (lookup nd ndsts) ++ [Player])
	| otherwise = (nd, fromJust $ lookup nd ndsts)

addStates ::
	Node -> [Node] -> [(Node, [NodeState])] -> [Node] -> [(Node, [NodeState])]
addStates pp vnds ndsts = map $ addStates1 pp vnds ndsts
	
knownCityNodes :: Node -> [Node] ->
	[(Node, [NodeState])] -> EdgeAlist -> [(Node, [NodeState])]
knownCityNodes pp vnds ndsts ea = addStates pp vnds ndsts $ knownNeighbors vnds ea

removeUnknownCops1 :: [Node] -> (Node, [(Node, Bool)]) -> (Node, [(Node, Bool)])
removeUnknownCops1 vnds (nd0, nds) = (nd0, map ruc nds)
	where
	ruc (nd, c) = (nd, c && nd `elem` vnds)

removeUnknownCops :: [Node] -> EdgeAlist -> EdgeAlist
removeUnknownCops vnds = map $ removeUnknownCops1 vnds

removeUnknownEdges :: [Node] -> (Node, [(Node, Bool)]) -> (Node, [(Node, Bool)])
removeUnknownEdges vnds (nd0, nds) = (nd0, filter ((`elem` vnds) . fst) nds)

onlyKnownEdges :: [Node] -> EdgeAlist -> EdgeAlist
onlyKnownEdges vnds ea = filter ((`elem` vnds) . fst) ea ++
	filter (not . null . snd) (map (removeUnknownEdges vnds) ea)

knownCityEdges :: [Node] -> EdgeAlist -> EdgeAlist
knownCityEdges vnds = removeUnknownCops vnds . onlyKnownEdges vnds

drawKnownCity :: Node -> [Node] -> [(Node, [NodeState])] -> EdgeAlist -> IO ()
drawKnownCity pp vnds nds ea = ugraphPng
	"known-city"
	(knownCityNodes pp vnds nds ea) 
	(knownCityEdges vnds ea)
