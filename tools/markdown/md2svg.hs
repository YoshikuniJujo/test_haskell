import Control.Monad
import System.Environment
import System.FilePath

import Parser
import SVG

main :: IO ()
main = do
	fp : as <- getArgs
	let size = case as of
		[s] -> read s
		_ -> 0.15
	cnt <- readFile fp
	case parseMrd cnt of
		Just t -> forM_ (zip [1 ..] $ textToSVG True size t) $ \(i, s) ->
			writeFile (mkSVGFileName fp i) s
		_ -> return ()

mkSVGFileName :: FilePath -> Int -> FilePath
mkSVGFileName fp i = (dropExtension fp ++ show2 i) <.> "svg"

show2 :: Show a => a -> String
show2 x = replicate (2 - length s) '0' ++ s
	where
	s = show x
