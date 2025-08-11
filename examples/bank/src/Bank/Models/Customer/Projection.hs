{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Projection
  ( Customer (..),
    CustomerEvent (..),
    customerProjection,
  )
where

import Bank.Json
import Bank.Models.Customer.Events
import Data.Aeson.TH
import Eventium
import SumTypesX.TH

newtype Customer
  = Customer
  { customerName :: Maybe String
  }
  deriving (Show, Eq)

deriveJSON (unPrefixLower "customer") ''Customer

constructSumType "CustomerEvent" (defaultSumTypeOptions {sumTypeOptionsTagOptions = AppendTypeNameToTags}) customerEvents

deriving instance Show CustomerEvent

deriving instance Eq CustomerEvent

handleCustomerEvent :: Customer -> CustomerEvent -> Customer
handleCustomerEvent customer (CustomerCreatedCustomerEvent (CustomerCreated name)) = customer {customerName = Just name}
handleCustomerEvent customer (CustomerCreationRejectedCustomerEvent _) = customer

customerProjection :: Projection Customer CustomerEvent
customerProjection = Projection (Customer Nothing) handleCustomerEvent
