{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ClientTools where

import Network.Socket
import Network.Socket.ByteString

import qualified Data.ByteString.Char8 as BSC

hints :: AddrInfo
hints = defaultHints { addrSocketType = Stream }

connectToServer :: IO Socket
connectToServer = do
	addr <- head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "3000")
	sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
	connect sock $ addrAddress addr
	pure sock

sendMessage :: Socket -> BSC.ByteString -> IO ()
sendMessage sock msg = sendAll sock msg
