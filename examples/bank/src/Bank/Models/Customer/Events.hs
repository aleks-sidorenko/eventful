{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Events
  ( customerEvents,
    CustomerCreated (..),
    CustomerCreationRejected (..),
  )
where

import Bank.Json
import Language.Haskell.TH (Name)

customerEvents :: [Name]
customerEvents =
  [ ''CustomerCreated,
    ''CustomerCreationRejected
  ]

newtype CustomerCreated
  = CustomerCreated
  { customerCreatedName :: String
  }
  deriving (Show, Eq)

newtype CustomerCreationRejected
  = CustomerCreationRejected
  { customerCreationRejectedReason :: String
  }
  deriving (Show, Eq)

deriveJSONUnPrefixLower ''CustomerCreated
deriveJSONUnPrefixLower ''CustomerCreationRejected
