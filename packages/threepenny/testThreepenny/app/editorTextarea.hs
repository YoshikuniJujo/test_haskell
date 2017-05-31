import Data.List
import System.Directory
import System.FilePath

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
	startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
	_ <- return window # set title "editor with textarea"
	ta <- UI.textarea #. "textarea"
		# set style [
			("width", "450px"), ("height", "400px") ]
	getBody window #+ [element ta]

	(cde, cdh) <- liftIO newEvent
	cdb <- stepper "/" cde
	showDirectory ta =<< currentValue cdb
	b <- liftIO $ stepper (- 1, - 1) (UI.mousemove ta)
	liftIO . register (UI.click ta) . const $ do
		y <- currentValue b
		i <- (`div` 69) . (* 4) . subtract 0 . snd <$> currentValue b
		print y
		print i
		fps <- getDirCont =<< currentValue cdb
		flip (maybe $ return ()) (fps `index` i) $ \fp -> do
			cdh . (</> fp) =<< currentValue cdb
			runUI window . showDirectory ta
				=<< currentValue cdb
		return ()
	return ()

showDirectory :: UI.Element -> FilePath -> UI ()
showDirectory ta fp = do
	fps <- liftIO $ getDirCont fp
	(return ta #) . set value $ unlines fps
	return ()

getDirCont :: FilePath -> IO [FilePath]
getDirCont fp =
	sort . filter (not . isPrefixOf ".") <$> getDirectoryContents fp

index :: [a] -> Int -> Maybe a
index (x : _) 0 = Just x
index [] _ = Nothing
index _ n | n < 0 = Nothing
index (_ : xs) n = index xs (n - 1)
