{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BlockHeader
  ( BlockHeader,
    createBlockHeader,
    getBlockHash,
    isValidBlockHeader,
    workForProof,
    index,
    timestamp,
    prevHash,
    proof,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import GHC.Generics

-- Data structure for a Block header
data BlockHeader = BlkHdr
  { index :: Int,
    timestamp :: Int,
    prevHash :: String,
    proof :: Int
  }
  deriving (Show, Eq, Generic)

-- Create a new block from index, timestamp, hash of the previous block and proof of work
createBlockHeader :: Int -> Int -> String -> Int -> BlockHeader
createBlockHeader index timestamp prevHash proof
  | (index < 0) = error "index cannot be negative"
  | (timestamp < 0) = error "timestamp cannot be negative"
  | otherwise = (BlkHdr {index = index, timestamp = timestamp, prevHash = prevHash, proof = proof})

-- Return the block hash. The hashing function in SHA256
getBlockHash :: BlockHeader -> String
getBlockHash blkHdr = show (hash (toStrict (encode blkHdr)) :: Digest SHA256)

-- Check if the block header hash is valid
isValidBlockHeader :: BlockHeader -> String -> Bool
isValidBlockHeader blkHdr targetDifficulty = (take (length targetDifficulty) (getBlockHash blkHdr)) == targetDifficulty

-- Find a proof that makes valid block header hash and return the block header
workForProof :: BlockHeader -> String -> BlockHeader
workForProof blkHdr targetDifficulty
  | isValidBlockHeader blkHdr targetDifficulty = blkHdr
  | otherwise = workForProof next_blkHdr targetDifficulty
  where
    next_blkHdr = blkHdr {proof = (proof blkHdr) + 1}

instance FromJSON BlockHeader

instance ToJSON BlockHeader where
  toEncoding blkHdr =
    pairs $
      "index" .= index blkHdr
        <> "timestamp" .= timestamp blkHdr
        <> "prevHash" .= prevHash blkHdr
        <> "proof" .= proof blkHdr