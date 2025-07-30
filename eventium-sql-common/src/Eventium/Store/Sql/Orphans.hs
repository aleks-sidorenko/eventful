{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Eventium.Store.Sql.Orphans
  (
  ) where

import Data.Proxy
import Data.UUID
import Database.Persist
import Database.Persist.Sql

import Eventium.Store.Class
import Eventium.UUID

instance PersistField UUID where
  toPersistValue = PersistText . uuidToText
  fromPersistValue (PersistText t) =
    case uuidFromText t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Expected PersistText for UUID"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PersistField EventVersion where
  toPersistValue = toPersistValue . unEventVersion
  fromPersistValue = fmap EventVersion . fromPersistValue

instance PersistFieldSql EventVersion where
  sqlType _ = sqlType (Proxy :: Proxy Int)

instance PersistField SequenceNumber where
  toPersistValue = toPersistValue . unSequenceNumber
  fromPersistValue = fmap SequenceNumber . fromPersistValue

instance PersistFieldSql SequenceNumber where
  sqlType _ = sqlType (Proxy :: Proxy Int)
