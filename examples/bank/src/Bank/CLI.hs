{-# LANGUAGE RecordWildCards #-}

module Bank.CLI
  ( bankCLIMain,
  )
where

import Bank.CLI.Options
import Bank.CLI.RunCommand
import Control.Monad.Logger (NoLoggingT (..), runNoLoggingT)
import Data.Text (pack)
import Database.Persist.Sqlite
import Eventium.Store.Sqlite

bankCLIMain :: IO ()
bankCLIMain = do
  Options {..} <- runOptionsParser

  -- Set up DB connection
  runNoLoggingT $ withSqlitePool (pack optionsDatabaseFile) 1 $ \pool -> NoLoggingT $ do
    initializeSqliteEventStore defaultSqlEventStoreConfig pool
    runCLICommand pool optionsCommand
