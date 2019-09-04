{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent (forkFinally)
import Control.Monad (unless, forever, void)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import qualified Control.Exception as Ex
import qualified Data.ByteString as BS

main :: IO ()
main = withSocketsDo $ do
	addr <- resolve "3000"
	Ex.bracket (open addr) close loop
	where
	resolve port = do
		let hints = defaultHints {
			addrFlags = [AI_PASSIVE],
			addrSocketType = Stream }
		addr : _ <- getAddrInfo (Just hints) Nothing (Just port)
		return addr
	open addr = do
		sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
		setSocketOption sock ReuseAddr 1
		withFdSocket sock $ setCloseOnExecIfNeeded
		bind sock (addrAddress addr)
		listen sock 10
		return sock
	loop sock = forever $ do
		(conn, peer) <- accept sock
		putStrLn $ "Connection from " ++ show peer
		void $ forkFinally (talk conn) (\_ -> close conn)
	talk conn = do
		msg <- recv conn 1024
		unless (BS.null msg) $ do
			sendAll conn msg
			talk conn
