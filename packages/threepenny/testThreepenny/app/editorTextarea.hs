import Control.Monad
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
	gub <- UI.button # set UI.text ".."
	getBody window #+ [element ta, element gub]

	(cde, cdh) <- liftIO newEvent
	cdb <- stepper "/" cde
	(ope, oph) <- liftIO newEvent
	opd <- stepper False ope
	showDirectory ta =<< currentValue cdb
	b <- liftIO $ stepper (- 1, - 1) (UI.mousemove ta)
	liftIO . register (UI.click ta) . const $ do
		o <- currentValue opd
		when (not o) $ clicked window ta cdb cdh b oph
	liftIO . register (UI.click gub) . const $ do
		cdh . goUp =<< currentValue cdb
		runUI window . showDirectory ta =<< currentValue cdb
		oph False
	return ()

clicked :: Window -> Element ->
	Behavior FilePath -> (FilePath -> IO ()) ->
	Behavior (Int, Int) -> (Bool -> IO ()) -> IO ()
clicked window ta cdb cdh b oph = do
		y <- currentValue b
		i <- (`div` 69) . (* 4) . subtract 0 . snd <$> currentValue b
		print y
		print i
		fps <- getDirCont =<< currentValue cdb
		flip (maybe $ return ()) (fps `index` i) $ \fp -> do
			fpn <- (</> fp) <$> currentValue cdb
			b <- doesDirectoryExist fpn
			if b
				then do	cdh fpn
					runUI window . showDirectory ta
						=<< currentValue cdb
				else if takeExtension fp `elem` [".hs"]
					then do	print $ "HS: " ++ fpn
						cdh fpn
						oph True
						runUI window
							$ (return ta #)
							. set value
							=<< liftIO (readFile fpn)
						return ()
					else return ()
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

goUp :: FilePath -> FilePath
goUp fp = case splitDirectories fp of
	[] -> fp
	["/"] -> fp
	ds -> joinPath $ init ds
