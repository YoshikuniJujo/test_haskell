{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module EchoClient where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C

echoClient :: IO ()
echoClient = runTCPClient "127.0.0.1" "3000" \s -> do
	sendAll s "Hello, world!"
	msg <- recv s 1024
	putStr "Received: "
	C.putStrLn msg

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
	addr <- resolve
	E.bracket (open addr) close client
	where
	resolve = do
		let hints = defaultHints { addrSocketType = Stream }
		head <$> getAddrInfo (Just hints) (Just host) (Just port)
	open addr = do
		sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
		connect sock $ addrAddress addr
		return sock
