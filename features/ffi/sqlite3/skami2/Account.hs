{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}

module Account (
	Connection, UserName(..), MailAddress(..), Password(..),
	UUID, NewAccountError(..), DeriveError(..),
	open, close, newTable, newAccount, removeAccount, activate,
	checkPassword, mailAddress,
	) where

import qualified Data.ByteString as BS
import qualified Database as DB
import Hash

data Connection = Connection {
	connection :: DB.SQLite,
	stmtNewAccount :: DB.Stmt,
	stmtRemoveAccount :: DB.Stmt,
	stmtAcctivate :: DB.Stmt,
	stmtGetSaltHash :: DB.Stmt,
	stmtMailAddress :: DB.Stmt
	} deriving Show

data UserName = UserName BS.ByteString deriving Show
data MailAddress = MailAddress BS.ByteString deriving Show
data UUID

data NewAccountError = UserNameAlreadyExist | MailAddressAlreadyExist deriving Show
data DeriveError = NoAccount | NotActivated deriving Show

open :: IO Connection
open = do
	conn <- DB.open
	sna <- DB.mkAccount conn
	sh <- DB.saltHash conn
	return $ Connection {
		connection = conn,
		stmtNewAccount = sna,
		stmtGetSaltHash = sh
		}

close :: Connection -> IO ()
close conn = do
	DB.finalizeStmt $ stmtNewAccount conn
	DB.finalizeStmt $ stmtGetSaltHash conn
	DB.close $ connection conn

newTable :: Connection -> IO ()
newTable = DB.newTable . connection

newAccount :: Connection ->
	UserName -> MailAddress -> Password -> IO (Maybe NewAccountError)
newAccount conn (UserName nm) (MailAddress addr) psw = do
	DB.bindStmt stmt "name" nm
	DB.bindStmt stmt "mail_address" addr
	(slt, hs) <- createHash psw
	setSalt stmt slt
	setHash stmt hs
	DB.runStmt stmt
	return Nothing
	where
	stmt = stmtNewAccount conn 

removeAccount :: Connection -> UserName -> IO ()
removeAccount = undefined

activate :: Connection -> UUID -> IO ()
activate = undefined

checkPassword :: Connection -> UserName -> Password -> IO Bool
checkPassword conn (UserName nm) psw = do
	(s, h) <- getSaltHash (stmtGetSaltHash conn) nm
	return $ checkHash psw s h

mailAddress :: Connection -> UserName -> IO (Either DeriveError MailAddress)
mailAddress = undefined
