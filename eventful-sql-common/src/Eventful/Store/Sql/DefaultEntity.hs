{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition for a default Entity to use with a SQL event store.

module Eventful.Store.Sql.DefaultEntity
  ( SqlEvent (..)
  , SqlEventId
  , migrateSqlEvent
  , defaultSqlEventStoreConfig
  ) where

import Database.Persist.TH

import Eventful.Store.Class
import Eventful.UUID

import Eventful.Store.Sql.Operations
import Eventful.Store.Sql.JSONString
import Eventful.Store.Sql.Orphans ()

share [mkPersist sqlSettings, mkMigrate "migrateSqlEvent"] [persistLowerCase|
SqlEvent sql=events
    Id SequenceNumber sql=sequence_number default=0 autoincrement
    uuid UUID
    version EventVersion
    event JSONString
    UniqueUuidVersion uuid version
    deriving Show
|]

defaultSqlEventStoreConfig :: SqlEventStoreConfig SqlEvent JSONString
defaultSqlEventStoreConfig =
  SqlEventStoreConfig
  SqlEvent
  SqlEventKey
  (\(SqlEventKey seqNum) -> seqNum)
  sqlEventUuid
  sqlEventVersion
  sqlEventEvent
  SqlEventId
  SqlEventUuid
  SqlEventVersion
  SqlEventEvent
