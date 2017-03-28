{-# LANgUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Template (
	Entity(..),
	tables, persistLowerCase, runDB, selectAll, deleteAll,
	put, newline
	) where

import Control.Monad.IO.Class

import Language.Haskell.TH

import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Exception

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Postgresql

import Database.PostgreSQL.Simple.Internal

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Control.Monad.Trans.Reader

tables :: String -> [EntityDef] -> DecsQ
tables ma = share [mkPersist sqlSettings, mkMigrate ma]

database :: BS.ByteString
database = "host=localhost port=5432 user=tatsuya dbname=mydb"

runDB :: Migration -> ReaderT SqlBackend (ResourceT (NoLoggingT IO)) a -> IO a
runDB mg m = catch (runNoLoggingT . runResourceT . withPostgresqlConn database
		. runSqlConn $ runMigration mg >> m) $
	\(e :: SqlError) -> do
		BSC.putStrLn $ sqlErrorMsg e
		throwIO e

put :: MonadIO m => String -> m ()
put = liftIO . putStr

newline :: MonadIO m => m ()
newline = liftIO $ putStrLn ""

selectAll :: forall t m backend . (PersistEntityBackend t ~ BaseBackend backend,
	PersistEntity t, PersistQueryRead backend,
	MonadIO m) => ReaderT backend m [Entity t]
selectAll = selectList [] []

deleteAll :: forall record m backend . (
	PersistEntityBackend record ~ BaseBackend backend,
	PersistEntity record,
	MonadIO m,
	PersistQueryWrite backend) => ReaderT backend m ()
deleteAll = deleteWhere @_ @_ @record []
