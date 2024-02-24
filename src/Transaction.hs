{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Transaction
  ( Transaction,
    createTransaction,
    sender,
    receiver,
    amount,
  )
where

import Data.Aeson
import GHC.Generics

data Transaction = Trans
  { sender :: String,
    receiver :: String,
    amount :: Int
  }
  deriving (Show, Eq, Generic)

-- Create a new transaction from sender, receiver and amount to be transferred
createTransaction :: String -> String -> Int -> Transaction
createTransaction sender receiver amount
  | (sender == receiver) = error "sender and receiver cannot be the same"
  | (amount < 0) = error "amount cannot be negative"
  | otherwise = (Trans {sender = sender, receiver = receiver, amount = amount})

instance FromJSON Transaction

instance ToJSON Transaction where
  toEncoding trans =
    pairs $
      "sender" .= sender trans
        <> "receiver" .= receiver trans
        <> "amount" .= amount trans
