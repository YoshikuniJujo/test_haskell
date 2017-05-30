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
			("width", "470px"), ("height", "300px"),
			("border", "solid black 1px"), ("overflow", "scroll") ]
		# set (attr "tabindex") "1"
	canvas <- UI.canvas #. "canvas"
		# set UI.textFont "20px sans-serif"
		# set UI.width 450
		# set UI.height 1800
		# set style [
			("width", "450px"), ("height", "1800px"),
			("overflow", "scroll"),
			("border", "solid black 1px") ]
		# set (attr "tabindex") "1"
	return canvas # set UI.textFont "30px sans-serif"
	get UI.textFont canvas >>= liftIO . print
	UI.fillText "hello" (10, 40) canvas
	UI.fillText "あいうえお漢字" (10, 80) canvas
	UI.strokeText "hello" (10, 120) canvas
	UI.strokeText "bottom" (10, 400) canvas
	files <- UI.button # set UI.text "Open File"
	return wrap #+ [element canvas]
	_ <- getBody window #+ [element wrap, element files]
	(cure, curh) <- liftIO newEvent
	curb <- stepper "/" cure
	_ <- liftIO . register (UI.click files) . const $ do
		runUI window . showDirectory canvas =<< currentValue curb
	b <- liftIO $ stepper (- 1, - 1) (UI.mousemove canvas)
	_ <- liftIO . register (UI.click canvas) . const $ do
		fps <- (sort <$>) $ getDirCont =<< currentValue curb
		print fps
		i <- (`div` 40) . subtract 5 . snd <$> currentValue b
		print i
		flip (maybe $ return ()) (fps `index` i) $ \fp -> do
			curh . (</> fp) =<< currentValue curb
			currentValue curb >>= print
		runUI window . showDirectory canvas =<< currentValue curb
	return ()

index :: [a] -> Int -> Maybe a
index (x : _) 0 = Just x
index [] _ = Nothing
index [] n | n < 0 = Nothing
index (_ : xs) n = index xs (n - 1)

showDirectory :: UI.Canvas -> FilePath -> UI ()
showDirectory c fp = do
	UI.clearCanvas c
	fps <- liftIO $ getDirCont fp
	showLines c 40 $ sort fps
--	UI.fillText (unwords $ sort fps) (10, 40) c

showLines :: UI.Canvas -> Double -> [String] -> UI ()
showLines c y (s : ss) = do
	UI.fillText s (10, y) c
	showLines c (y + 40) ss
showLines _ _ [] = return ()
