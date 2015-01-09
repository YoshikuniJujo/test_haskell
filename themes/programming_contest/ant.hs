import Control.Arrow

type Ant = Int
type StickLen = Int

examAnts :: [Ant]
examAnts = [8, 4, 2, 5, 1, 3]

stickLen :: StickLen
stickLen = 10

dropMin, dropMax :: StickLen -> Ant -> Int
dropMin l ant = min ant $ l - ant
dropMax l ant = max ant $ l - ant

minTime, maxTime :: Int -> [Int] -> Int
minTime l = maximum . map (dropMin l)
maxTime l = maximum . map (dropMax l)

minMaxTime :: Int -> [Int] -> (Int, Int)
minMaxTime = curry $ uncurry minTime &&& uncurry maxTime
