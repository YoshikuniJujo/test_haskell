{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ServerTools (listen3000, withConnection, close) where

import System.Posix.Types
import Network.Socket
import Network.Socket.ByteString

import qualified Data.ByteString.Char8 as BSC

hints :: AddrInfo
hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }

listen3000 :: IO Socket
listen3000 = do
	addr <- head <$> getAddrInfo (Just hints) Nothing (Just "3000")
	sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
	setSocketOption sock ReuseAddr 1
	withFdSocket sock $ setCloseOnExecIfNeeded
	bind sock $ addrAddress addr
	listen sock 1024
	pure sock

getConnection :: Socket -> IO Socket
getConnection sock = do
	(conn, _peer) <- accept sock
	pure conn

withConnection :: Socket -> (Fd -> IO r) -> IO r
withConnection sock act = do
	conn <- getConnection sock
	withFdSocket conn $ act . Fd

sample1 :: IO ()
sample1 = do
	l <- listen3000
	c1 <- getConnection l
	c2 <- getConnection l
	msg1 <- recv c1 1024
	msg2 <- recv c2 1024
	BSC.putStrLn msg1
	BSC.putStrLn msg2
