{-# LANGUAGE OverloadedStrings #-}

module Example where

import Lib

password = "nostr"

logN = 16

getSaltSymmetricKey = scryptIO logN password

example = do
	putStrLn $ "PASSWORD = " ++ show password
	putStrLn $ "LOG_N = " ++ show logN
	Scrypted { salt = slt, pass = sk } <- getSaltSymmetricKey
	putStrLn $ "SALT = " ++ show slt
	putStrLn $ "SYMMETRIC_KEY = " ++ show sk

encrypted = "ncryptsec1qgg9947rlpvqu76pj5ecreduf9jxhselq2nae2kghhvd5g7dgjtcxfqtd67p9m0w57lspw8gsq6yphnm8623nsl8xn9j4jdzz84zm3frztj3z7s35vpzmqf6ksu8r89qk5z2zxfmu5gv8th8wclt0h4p"
