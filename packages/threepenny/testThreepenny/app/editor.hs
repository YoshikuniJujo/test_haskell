import Control.Monad
import System.Directory
import Data.List
import System.FilePath

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
	startGUI defaultConfig setup

getDirCont :: FilePath -> IO [FilePath]
getDirCont fp = filter (not . isPrefixOf ".") <$> getDirectoryContents fp

setup :: Window -> UI ()
setup window = do
	_ <- return window # set title "editor"
	wrap <- UI.div #. "wrap"
		# set style [
			("width", "920px"), ("height", "580px"),
			("border", "solid black 1px"), ("overflow", "scroll") ]
		# set (attr "tabindex") "1"
	canvas <- UI.canvas #. "canvas"
		# set UI.textFont "20px sans-serif"
		# set UI.width 900
		# set UI.height 1800
		# set style [
			("width", "900px"), ("height", "1800px"),
			("overflow", "scroll"),
			("border", "solid black 1px") ]
		# set (attr "tabindex") "1"
	return canvas # set UI.textFont "20px sans-serif"
	get UI.textFont canvas >>= liftIO . print
	files <- UI.button # set UI.text ".."
	return wrap #+ [element canvas]
	_ <- getBody window #+ [element wrap, element files]
	(cure, curh) <- liftIO newEvent
	curb <- stepper "/" cure
	showDirectory canvas =<< currentValue curb
	_ <- liftIO . register (UI.click files) . const $ do
		curh . goUp =<< currentValue curb
		runUI window . showDirectory canvas =<< currentValue curb
	b <- liftIO $ stepper (- 1, - 1) (UI.mousemove canvas)
	_ <- liftIO . register (UI.click canvas) . const $ do
		fps <- (sort <$>) $ getDirCont =<< currentValue curb
		print fps
		i <- (`div` 30) . subtract 5 . snd <$> currentValue b
		print i
		flip (maybe $ return ()) (fps `index` i) $ \fp -> do
			fpn <- (</> fp) <$> currentValue curb
			b <- doesDirectoryExist fpn
			if b
				then do	curh . (</> fp) =<< currentValue curb
					runUI window . showDirectory canvas
						=<< currentValue curb
				else if takeExtension fp `elem` [".txt", ".hs"]
					then do	print $ "TEXT: " ++ fpn
						runUI window . showText canvas
							=<< readFile fpn
					else return ()
			currentValue curb >>= print
	return ()

showText :: UI.Canvas -> String -> UI ()
showText c s = do
	UI.clearCanvas c
	showLines c 30 $ lines s
--	UI.fillText s (10, 30) c

index :: [a] -> Int -> Maybe a
index (x : _) 0 = Just x
index [] _ = Nothing
index [] n | n < 0 = Nothing
index (_ : xs) n = index xs (n - 1)

showDirectory :: UI.Canvas -> FilePath -> UI ()
showDirectory c fp = do
	UI.clearCanvas c
	fps <- liftIO $ getDirCont fp
	showLines c 30 $ sort fps
--	UI.fillText (unwords $ sort fps) (10, 30) c

goUp :: FilePath -> FilePath
goUp fp = case splitDirectories fp of
	[] -> fp
	["/"] -> fp
	ds -> joinPath $ init ds

showLines :: UI.Canvas -> Double -> [String] -> UI ()
showLines c y (s : ss) = do
	UI.fillText s (10, y) c
	showLines c (y + 30) ss
showLines _ _ [] = return ()
