{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Blockchain
  ( Blockchain,
    Block,
    createGenesis,
    addNewBlock,
    mineBlock,
    targetDifficulty,
    requestNewTransaction,
    isValidChain,
    getLength,
    getLastBlock,
  )
where

import BlockHeader
import Data.Aeson
import Debug.Trace
import GHC.Generics
import Transaction

type Block = (BlockHeader, [Transaction])

data Blockchain = BC
  { pool :: [Transaction],
    blocks :: [Block]
  }
  deriving (Show, Eq, Generic)

targetDifficulty :: String
targetDifficulty = "0000"

-- Create a genesis block
createGenesis :: Blockchain
createGenesis = addNewBlock (BC {pool = [], blocks = []}) 0 "100" 1

-- Add a transaction to the pool of transactions to be processed
requestNewTransaction :: Blockchain -> Transaction -> (Blockchain, Int)
requestNewTransaction (BC {blocks = []}) _ = error "Cannot add transaction to an empty blockchain"
requestNewTransaction bc trans = (newBc, nextIndex)
  where
    newBc = BC {pool = newPool, blocks = blks}
    newPool = (pool bc) ++ [trans]
    blks = blocks bc
    nextIndex = (index (fst (last blks))) + 1

addNewBlock :: Blockchain -> Int -> String -> Int -> Blockchain
addNewBlock bc timestamp prevHash proof = (BC {pool = [], blocks = (blks ++ [newBlk])})
  where
    blks = blocks bc
    newBlkHdr = createBlockHeader ((length blks) + 1) timestamp prevHash proof
    newBlk = (newBlkHdr, (pool bc))

-- Create a new block from timestamp and mines it to make a valid block
mineBlock :: Blockchain -> Int -> Blockchain
mineBlock bc timestamp = (BC {pool = [], blocks = (blks ++ [newBlk])})
  where
    blks = blocks bc
    lastBlkHdr = fst (getLastBlock bc)
    newBlkHdr = workForProof (createBlockHeader ((length blks) + 1) timestamp (getBlockHash lastBlkHdr) 0) targetDifficulty
    newBlk = (newBlkHdr, (pool bc))

-- Check if the given blockchain is valid
isValidChain :: Blockchain -> Bool
isValidChain bc = isValidBlocks (blocks bc)

-- Check if the given list of blocks are vaid
isValidBlocks :: [Block] -> Bool
isValidBlocks [] = False
isValidBlocks (blk : []) = True
isValidBlocks (prevBlock : currentBlock : rest) =
  hashCheck && proofCheck && (isValidBlocks (currentBlock : rest))
  where
    hashCheck = getBlockHash (fst prevBlock) == prevHash (fst currentBlock)
    proofCheck = isValidBlockHeader (fst currentBlock) targetDifficulty

-- Return the length of this blockchain
getLength :: Blockchain -> Int
getLength bc = length (blocks bc)

-- Retrieve the last block in the chain
getLastBlock :: Blockchain -> Block
getLastBlock (BC {blocks = []}) = error "Cannot retrieve the last block from an empty blockchain"
getLastBlock bc = last (blocks bc)

instance FromJSON Blockchain

instance ToJSON Blockchain where
  toEncoding bc =
    pairs $
      "blocks" .= blocks bc
        <> "pool" .= pool bc