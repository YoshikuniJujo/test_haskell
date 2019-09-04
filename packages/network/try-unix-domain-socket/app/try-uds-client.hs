{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Posix.Process

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

import qualified Control.Exception as Ex
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = withSocketsDo $ do
	getProcessID >>= print
	sock <- socket AF_UNIX Stream 0
	Ex.bracket (open sock) close talk
	where
	open sock = do
		connect sock $ SockAddrUnix "/tmp/foo.sock"
		return sock
	talk sock = do
		sendAll sock "Hello, world!"
		msg <- recv sock 1024
		putStr "Received: "
		BSC.putStrLn msg
