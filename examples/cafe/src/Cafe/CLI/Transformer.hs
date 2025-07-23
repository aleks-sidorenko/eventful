module Cafe.CLI.Transformer
  ( CLI
  , runCLI
  , runDB
  , cliEventStore
  , cliEventStoreReader
  , cliGloballyOrderedEventStore
  ) where

import Control.Monad.Reader
import Database.Persist.Sql

import Eventful
import Eventful.Store.Sqlite

type CLI = ReaderT ConnectionPool IO

-- | Main entry point into a CLI program.
runCLI :: ConnectionPool -> CLI a -> IO a
runCLI pool action' = runReaderT action' pool

-- | Run a given database action by using the connection pool from the Reader
-- monad.
runDB :: SqlPersistT IO a -> CLI a
runDB query = do
  pool <- ask
  liftIO $ runSqlPool query pool

cliEventStore :: (MonadIO m) => VersionedEventStoreWriter (SqlPersistT m) JSONString
cliEventStore = sqliteEventStoreWriter defaultSqlEventStoreConfig

cliEventStoreReader :: (MonadIO m) => VersionedEventStoreReader (SqlPersistT m) JSONString
cliEventStoreReader = sqlEventStoreReader defaultSqlEventStoreConfig

cliGloballyOrderedEventStore :: (MonadIO m) => GlobalEventStoreReader (SqlPersistT m) JSONString
cliGloballyOrderedEventStore = sqlGlobalEventStoreReader defaultSqlEventStoreConfig
