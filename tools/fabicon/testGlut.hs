import Graphics.UI.GLUT

main :: IO ()
main = do
	initialize "hoge" []
	initialDisplayMode $= [RGBAMode, DoubleBuffered]
	initialWindowSize $= Size 640 480
	top <- createWindow "hello"
	cursor $= LeftArrow
--	addTimerCallback 10000 $ destroyWindow top
	displayCallback $= display
	reshapeCallback $= Just reshape
	keyboardCallback $= Just (onPressKey top)
	addTimerCallback 2000 $ display
	mainLoop

display :: IO ()
display = do
	clearColor $= Color4 1 1 1 1
	clear [ColorBuffer]
	loadIdentity
	preservingMatrix $ do
--	do
		rotate 0 (Vector3 0 0 1 :: Vector3 GLdouble)
		renderPrimitive Quads $ mapM_ vertex [
			Vertex3 0.10 0.10 0.0,
			Vertex3 (-0.10) 0.10 0.0,
			Vertex3 (-0.10) (-0.10) 0.0 :: Vertex3 GLfloat,
			Vertex3 0.10 (-0.10) 0.0
			]
	swapBuffers
	flush
	putStrLn "display"

onPressKey :: Window -> Char -> Position -> IO ()
onPressKey w 'q' _ = putStrLn "quit" >> destroyWindow w
onPressKey _ c p = do
	print c
	print p

reshape size@(Size w h) = do
	viewport $= (Position 0 0, size)
	matrixMode $= Projection
	loadIdentity
	perspective 60.0 (fromIntegral w / fromIntegral h) 0.001 50.0
--	lookAt (Vertex3 0 0 (- 1)) (Vertex3 0 0 0) (Vertex3 0 1 0)
	matrixMode $= Modelview 0
