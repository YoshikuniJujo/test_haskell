{-# LANGUAGE ViewPatterns, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
import System.PortAudio
import Control.Concurrent
import Control.Monad
import Linear (V2(..))
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MV
import Options.Applicative

period :: Int
period = 128

table :: V.Vector Float
table = V.fromList [sin t | i <- [0..period - 1], let t = fromIntegral i / fromIntegral period * 2 * pi]

callback :: MVar Int -> Status -> input -> MV.IOVector (V2 Float) -> IO StreamCallbackResult
callback phase _ _ o = do
  i0 <- takeMVar phase
  go i0 0
  putMVar phase $ i0 + n
  return Continue
  where
    n = MV.length o
    go :: Int -> Int -> IO ()
    go i0 i
      | i == n = return ()
      | otherwise = do
        let v = table V.! ((i0 + i) `mod` period)
        MV.write o i (V2 v v)
        go i0 (i + 1)

main :: IO ()
main = join $ execParser (info app mempty)

app :: Parser (IO ())
app = do
  rate <- option auto $ long "rate" <> help "sampling rate" <> value 48000
  buf <- option auto $ long "buffer" <> help "number of samples for the buffer" <> value 1024
  device <- option auto $ long "device" <> help "device index" <> value (-1)
  helper
  pure $ withPortAudio $ do
    (_, devs) <- getDevices
    if device < 0
      then forM_ (zip [0 :: Int ..] devs) $ \(i, dev) ->
        putStrLn $ show i ++ ": " ++ deviceName dev
      else do
        let dev = devs !! device
        phase <- newMVar 0
        let output = streamParameters dev 0
        withStream rate buf noConnection output mempty (callback phase)
          $ \s -> do
            setStreamFinishedCallback s $ putStrLn "Done"
            withStartStream s $ threadDelay $ 1000 * 1000
