{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, PackageImports #-}

module Account (
	Connection, UserName(..), MailAddress(..), Password(..),
	UUID, NewAccountError(..), DeriveError(..),
	open, close, newTable, newAccount, removeAccount, activate,
	checkPassword, mailAddress,
	) where

import qualified Data.ByteString as BS
import Data.IORef
import "crypto-random" Crypto.Random

import qualified Database as DB
import Hash
import UUID4

data Connection = Connection {
	connection :: DB.SQLite,
	connCprg :: IORef SystemRNG,
	stmtNewAccount :: DB.Stmt,
	stmtCheckName :: DB.Stmt,
	stmtCheckAddress :: DB.Stmt,
	stmtRemoveAccount :: DB.Stmt,
	stmtGetAcctive :: DB.Stmt,
	stmtSetAcctive :: DB.Stmt,
	stmtGetSaltHash :: DB.Stmt,
	stmtMailAddress :: DB.Stmt
	}

data UserName = UserName BS.ByteString deriving Show
data MailAddress = MailAddress BS.ByteString deriving Show
data UUID

data NewAccountError = UserNameAlreadyExist | MailAddressAlreadyExist deriving Show
data DeriveError = NoAccount | NotActivated deriving Show

open :: IO Connection
open = do
	conn <- DB.open
	ep <- createEntropyPool
	cc <- newIORef $ cprgCreate ep
	sna <- DB.mkAccount conn
	cn <- DB.checkName conn
	ca <- DB.checkAddress conn
	sh <- DB.saltHash conn
	return $ Connection {
		connection = conn,
		connCprg = cc,
		stmtNewAccount = sna,
		stmtCheckName = cn,
		stmtCheckAddress = ca,
		stmtGetSaltHash = sh
		}

close :: Connection -> IO ()
close conn = do
	DB.finalizeStmt $ stmtNewAccount conn
	DB.finalizeStmt $ stmtCheckName conn
	DB.finalizeStmt $ stmtCheckAddress conn
	DB.finalizeStmt $ stmtGetSaltHash conn
	DB.close $ connection conn

newTable :: IO ()
newTable = do
	conn <- DB.open
	DB.newTable conn
	DB.close conn

checkName :: Connection -> UserName -> IO Bool
checkName conn (UserName nm) = do
	DB.bindStmt stmt "name" nm
	DB.existStmt stmt
	where stmt = stmtCheckName conn 

checkAddress :: Connection -> MailAddress -> IO Bool
checkAddress conn (MailAddress ma) = do
	DB.bindStmt stmt "mail_address" ma
	DB.existStmt stmt
	where stmt = stmtCheckAddress conn 

newAccount :: Connection ->
	UserName -> MailAddress -> Password -> IO (Maybe NewAccountError)
newAccount conn un@(UserName nm) ma@(MailAddress addr) psw = do
	ne <- checkName conn un
	ae <- checkAddress conn ma
	case (ne, ae) of
		(True, _) -> return $ Just UserNameAlreadyExist
		(_, True) -> return $ Just MailAddressAlreadyExist
		_ -> do
			(UUID4 uu) <- uuid4IO $ connCprg conn
			DB.bindStmt stmt "name" nm
			DB.bindStmt stmt "mail_address" addr
			DB.bindStmt stmt "act_key" uu
			(slt, hs) <- createHash psw
			setSalt stmt slt
			setHash stmt hs
			DB.runStmt stmt
			return Nothing
	where stmt = stmtNewAccount conn 

removeAccount :: Connection -> UserName -> IO ()
removeAccount = undefined

activate :: Connection -> UUID -> IO ()
activate = undefined

checkPassword :: Connection -> UserName -> Password -> IO Bool
checkPassword conn (UserName nm) psw = do
--	DB.bindStmt (stmtCheckName conn) "name" nm
--	stmtExist $ stmtCheckName conn
	(s, h) <- getSaltHash (stmtGetSaltHash conn) nm
	return $ checkHash psw s h

mailAddress :: Connection -> UserName -> IO (Either DeriveError MailAddress)
mailAddress = undefined
