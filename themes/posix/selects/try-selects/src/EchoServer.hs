{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module EchoServer where

import Control.Monad
import Control.Concurrent
import Network.Socket
import Network.Socket.ByteString

import qualified Control.Exception as E
import qualified Data.ByteString as S

echoServer :: IO ()
echoServer = runTCPServer Nothing "3000" talk
	where
	talk s = do
		msg <- recv s 1024
		unless (S.null msg) $ do
			sendAll s msg
			talk s

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
	addr <- resolve
	E.bracket (open addr) close loop
	where
	resolve = do
		let hints = defaultHints {
			addrFlags = [AI_PASSIVE],
			addrSocketType = Stream }
		head <$> getAddrInfo (Just hints) mhost (Just port)
	open addr = do
		sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
		setSocketOption sock ReuseAddr 1
		withFdSocket sock $ setCloseOnExecIfNeeded
		bind sock $ addrAddress addr
		listen sock 1024
		return sock
	loop sock = forever $ do
		(conn, _peer) <- accept sock
		void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
