import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
	startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
	liftIO $ putStrLn "begin"
	_ <- return window # set title "test canvas"
	wrap <- UI.div #. "wrap"
		# set style [("width", "300px"), ("height", "300px"), ("border", "solid black 1px"), ("overflow", "scroll")]
		# set (attr "tabindex") "1"
	getBody window #+ [element wrap]
	b <- liftIO $ stepper (0, 0) (UI.mousemove wrap)
	_ <- liftIO . register (UI.click wrap) . const $ do
		currentValue b >>= print
	-- hoge
	{-
	on UI.mousemove wrap $ \xy ->
		liftIO $ print xy
		-}
	return ()
