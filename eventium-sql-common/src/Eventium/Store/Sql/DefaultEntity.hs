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
module Eventium.Store.Sql.DefaultEntity
  ( SqlEvent (..),
    SqlEventId,
    migrateSqlEvent,
    defaultSqlEventStoreConfig,
  )
where

import Database.Persist (Key)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Persist.TH
import Eventium.Store.Class
import Eventium.Store.Sql.JSONString
import Eventium.Store.Sql.Operations
import Eventium.Store.Sql.Orphans ()
import Eventium.UUID

share
  [mkPersist sqlSettings, mkMigrate "migrateSqlEvent"]
  [persistLowerCase|
SqlEvent sql=events
    uuid UUID
    version EventVersion
    event JSONString
    UniqueUuidVersion uuid version
    deriving Show
|]

sqlEventMakeKey :: SequenceNumber -> Key SqlEvent
sqlEventMakeKey sequenceNumber =
  toSqlKey (fromIntegral (unSequenceNumber sequenceNumber))

sqlEventUnKey :: Key SqlEvent -> SequenceNumber
sqlEventUnKey key =
  SequenceNumber (fromIntegral (fromSqlKey key))

defaultSqlEventStoreConfig :: SqlEventStoreConfig SqlEvent JSONString
defaultSqlEventStoreConfig =
  SqlEventStoreConfig
    SqlEvent
    sqlEventMakeKey
    sqlEventUnKey
    sqlEventUuid
    sqlEventVersion
    sqlEventEvent
    SqlEventId
    SqlEventUuid
    SqlEventVersion
    SqlEventEvent
