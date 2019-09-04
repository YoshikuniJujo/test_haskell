{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

import qualified Control.Exception as Ex
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = withSocketsDo $ do
	addr <- resolve "127.0.0.1" "3000"
	Ex.bracket (open addr) close talk
	where
	resolve host port = do
		let hints = defaultHints { addrSocketType = Stream }
		addr : _ <- getAddrInfo (Just hints) (Just host) (Just port)
		return addr
	open addr = do
		sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
		connect sock $ addrAddress addr
		return sock
	talk sock = do
		sendAll sock "Hello, world!"
		msg <- recv sock 1024
		putStr "Received: "
		BSC.putStrLn msg
