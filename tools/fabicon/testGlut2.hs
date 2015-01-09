import Graphics.UI.GLUT

main :: IO ()
main = do
	initialize "hoge" []
	top <- createWindow "hello"
	keyboardCallback $= Just (onPressKey top)
	displayCallback $= display
	reshapeCallback $= Just (const display)
	mainLoop

onPressKey :: Window -> Char -> Position -> IO ()
onPressKey w 'q' _ = putStrLn "quit" >> destroyWindow w
onPressKey _ _ _ = return ()

display :: IO ()
display = do
	clearColor $= Color4 1 1 1 1
	clear [ColorBuffer]
	rect (Vertex2 10 10 :: Vertex2 GLdouble) (Vertex2 100 100)
	flush
	putStrLn "display"
