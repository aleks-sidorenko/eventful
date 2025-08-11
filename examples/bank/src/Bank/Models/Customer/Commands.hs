{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Commands
  ( customerCommands,
    CreateCustomer (..),
  )
where

import Bank.Json
import Language.Haskell.TH (Name)

customerCommands :: [Name]
customerCommands =
  [ ''CreateCustomer
  ]

newtype CreateCustomer
  = CreateCustomer
  { createCustomerData :: String
  }
  deriving (Show, Eq)

deriveJSONUnPrefixLower ''CreateCustomer
