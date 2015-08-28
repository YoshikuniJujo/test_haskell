{-# LANGUAGE TupleSections #-}

import Data.Char

main :: IO [Int]
main = do
	src <- getContents
	brainkanji ([], [0]) ("", src)

brainkanji :: ([Int], [Int]) -> (String, String) -> IO [Int]
brainkanji m (_, "") = return $ memory m
brainkanji m (p, '進' : s) = brainkanji (fwd m) ('進' : p, s)
brainkanji m (p, '退' : s) = brainkanji (bwd m) ('退' : p, s)
brainkanji m (p, '増' : s) = brainkanji (scc m) ('増' : p, s)
brainkanji m (p, '減' : s) = brainkanji (prd m) ('減' : p, s)
brainkanji m (p, '書' : s) = put m >> brainkanji m ('書' : p, s)
brainkanji m (p, '読' : s) = get m >>= flip brainkanji ('読' : p, s)
brainkanji m (p, '始' : s) =
	if test m then brainkanji m s' else brainkanji m (jumpFwd 1 s')
	where s' = ('始' : p, s)
brainkanji m s'@(p, '終' : s) =
	if test m then brainkanji m (jumpBwd 1 s') else brainkanji m ('終' : p, s)
brainkanji m (p, c : s) = brainkanji m (p, s)

memory :: ([Int], [Int]) -> [Int]
memory (mps, ms) = reverse mps ++ ms

fwd, scc, prd :: ([Int], [Int]) -> ([Int], [Int])
fwd (mps, [m]) = (m : mps, [0])
fwd (mps, m : ms) = (m : mps, ms)
bwd (mp : mps, ms) = (mps, mp : ms)
bwd _ = error ""
scc (mps, m : ms) = (mps, succ m : ms)
prd (mps, m : ms) = (mps, pred m : ms)

put :: ([Int], [Int]) -> IO ()
put (_, m : _) = putChar $ chr m

get :: ([Int], [Int]) -> IO ([Int], [Int])
get (mps, _ : ms) = getChar >>= return . (mps ,) . (: ms) . ord

test :: ([Int], [Int]) -> Bool
test (_, 0 : _) = False
test _ = True

jumpFwd, jumpBwd :: Int -> (String, String) -> (String, String)
jumpFwd 0 s = s
jumpFwd n (p, '始' : s) = jumpFwd (n + 1) ('始' : p, s)
jumpFwd n (p, '終' : s) = jumpFwd (n - 1) ('終' : p, s)
jumpFwd n (p, c : s) = jumpFwd n (c : p, s)
jumpBwd 0 s = s
jumpBwd n ('始' : p, s) = jumpBwd (n - 1) (p, '始' : s)
jumpBwd n ('終' : p, s) = jumpBwd (n + 1) (p, '終' : s)
jumpBwd n (c : p, s) = jumpBwd n (p, c : s)
