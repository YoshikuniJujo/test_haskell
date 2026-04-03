{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KeyLog (KeyLog(..), writeKeyLog, readKeyLog) where

import Graphics.UI.GLFW qualified as Glfw

data KeyLog = KeyLog {
	klTime :: Double,
	klKey :: Glfw.Key,
	klAction :: Glfw.KeyState }
	deriving Show

writeKeyLog :: FilePath -> [KeyLog] -> IO ()
writeKeyLog fp = writeFile fp . unlines . (keyLogToText <$>)

readKeyLog :: FilePath -> IO [KeyLog]
readKeyLog fp = (textToKeyLog <$>) . lines <$> readFile fp

keyLogToText :: KeyLog -> String
keyLogToText KeyLog { klTime = t, klKey = k, klAction = a } =
	show t ++ " " ++ show k ++ " " ++ show a

textToKeyLog :: String -> KeyLog
textToKeyLog str = case words str of
	[t, k, a] -> KeyLog (read t) (read k) (read a)
	_ -> error "bad"
