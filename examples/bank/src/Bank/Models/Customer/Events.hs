{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Events
  ( customerEvents
  , CustomerCreated (..)
  , CustomerCreationRejected (..)
  ) where

import Language.Haskell.TH (Name)

import Bank.Json

customerEvents :: [Name]
customerEvents =
  [ ''CustomerCreated
  , ''CustomerCreationRejected
  ]

newtype CustomerCreated =
  CustomerCreated
  { customerCreatedName :: String
  } deriving (Show, Eq)

newtype CustomerCreationRejected
  = CustomerCreationRejected
  { customerCreationRejectedReason :: String
  } deriving (Show, Eq)

deriveJSONUnPrefixLower ''CustomerCreated
deriveJSONUnPrefixLower ''CustomerCreationRejected
