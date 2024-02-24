import BlockHeader
import Blockchain
import Control.Exception.Base
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Test.Hspec
import Transaction

user1 = "bc1p8xaxnnf8g9e05fuas7pzcc3x4z9dknt04pscr4s7yhv4mh6p5p3saw6p25"

user2 = "bc1pndj8khrm0gsq7ex37q0q8qkmhvznmn69r7sulqpvj8gnvf79qqzsqlygqz"

user3 = "bc1ppz3p5tleu80ks29flxyjj4ac2v6w4dsrqhyffupl6q54me7s0rzqafqedk"

transaction1 = createTransaction user1 user2 1

transaction2 = createTransaction user2 user3 100

transaction_NG_1 = createTransaction user2 user3 (-5)

transaction_NG_2 = createTransaction user2 user2 10

index1 = 2

timestamp1 = 100000

prevHash1 = "8e57cad3af7ea303593407123d89b476291fbc6a18373b7bade20adca983f607"

proof1 = 1298457

blockHdr1 = createBlockHeader index1 timestamp1 prevHash1 proof1

blockHdr_NG_1 = createBlockHeader (-1) timestamp1 prevHash1 proof1

blockHdr_NG_2 = createBlockHeader index1 (-1) prevHash1 proof1

bc1 = createGenesis

bc1_blockHdr1 = createBlockHeader 1 0 "100" 1 -- genesis block

bc1_blockHdr1_Hash = show (hash (toStrict (encode bc1_blockHdr1)) :: Digest SHA256)

(bc1_transaction1, bc1_nextIndex) = requestNewTransaction bc1 transaction1

bc2 = addNewBlock bc1_transaction1 timestamp1 prevHash1 proof1

bc2_blockHdr = blockHdr1

bc2_blockHdr_Hash = show (hash (toStrict (encode bc2_blockHdr)) :: Digest SHA256)

index2 = 3

timestamp2 = 200000

prevHash2 = "ea46b7fb79f58801b86f2ab5f1d75437ecc5998deca81349ee5d787668d4ef9d"

proof2 = 9999333

(bc2_transaction1, bc2_nextIndex1) = requestNewTransaction bc2 transaction1

(bc2_transaction2, bc2_nextIndex2) = requestNewTransaction bc2_transaction1 transaction2

bc3 = addNewBlock bc2_transaction2 timestamp2 prevHash2 proof2

bc3_blockHdr = createBlockHeader index2 timestamp2 prevHash2 proof2

bc3_blockHdr_Hash = show (hash (toStrict (encode bc3_blockHdr)) :: Digest SHA256)

b1 = createGenesis

(b1_t1, b1_next) = requestNewTransaction b1 transaction1

b2 = mineBlock b1_t1 timestamp1

(b2_t1, b2_next1) = requestNewTransaction b2 transaction1

(b2_t2, b2_next2) = requestNewTransaction b2_t1 transaction2

b3 = mineBlock b2_t2 timestamp2

b3_fake1 = addNewBlock b2_t2 timestamp2 (getBlockHash (fst (getLastBlock b2))) proof2

b3_fake2 = addNewBlock b2_t2 timestamp2 prevHash2 (proof (fst (getLastBlock b2)))

b3_fake3 = addNewBlock b2_t2 timestamp1 (getBlockHash (fst (getLastBlock b2))) (proof (fst (getLastBlock b2)))

main :: IO ()
main = hspec $ do
  describe "Test Transaction" $ do
    it "return the sender's name" $
      sender transaction1 `shouldBe` user1

    it "return the receiver's name" $
      receiver transaction1 `shouldBe` user2

    it "return the amount" $
      amount transaction1 `shouldBe` 1

    it "return the sender's name" $
      sender transaction2 `shouldBe` user2

    it "return the receiver's name" $
      receiver transaction2 `shouldBe` user3

    it "return the amount" $
      amount transaction2 `shouldBe` 100

    it "check error for negative amount" $
      evaluate (transaction_NG_1) `shouldThrow` anyErrorCall

    it "check error for having sender and reciver the same" $
      evaluate (transaction_NG_2) `shouldThrow` anyErrorCall

  describe "Test BlockHeader" $ do
    it "return the sender's name" $
      index blockHdr1 `shouldBe` index1

    it "return the receiver's name" $
      timestamp blockHdr1 `shouldBe` timestamp1

    it "return the amount" $
      proof blockHdr1 `shouldBe` proof1

    it "return the sender's name" $
      prevHash blockHdr1 `shouldBe` prevHash1

    it "check error for negative amount" $
      evaluate (blockHdr_NG_1) `shouldThrow` anyErrorCall

    it "check error for having sender and reciver the same" $
      evaluate (blockHdr_NG_2) `shouldThrow` anyErrorCall

  describe "Test BlockChain" $ do
    it "check the gensis block" $
      getLastBlock bc1 `shouldBe` (bc1_blockHdr1, [])

    it "return the length of the gensis block" $
      getLength bc1 `shouldBe` 1

    it "check the last block" $
      getLastBlock bc2 `shouldBe` (bc2_blockHdr, [transaction1])

    it "return the length of the block to be 2" $
      getLength bc2 `shouldBe` 2

    it "the third block will have two transactions" $
      getLastBlock bc3 `shouldBe` (bc3_blockHdr, [transaction1, transaction2])

    it "return the length of the block to be 3" $
      getLength bc3 `shouldBe` 3

  describe "Test Block Header Hash " $ do
    it "test bc1 BlockHash" $
      getBlockHash bc1_blockHdr1 `shouldBe` bc1_blockHdr1_Hash

    it "test bc2 BlockHash" $
      getBlockHash bc2_blockHdr `shouldBe` bc2_blockHdr_Hash

    it "test bc3 BlockHash" $
      getBlockHash bc3_blockHdr `shouldBe` bc3_blockHdr_Hash

  describe "Test chain of blocks and proof of work" $ do
    it "test b2's previous hash" $
      (prevHash (fst (getLastBlock b2))) `shouldBe` (getBlockHash (fst (getLastBlock b1)))

    it "test b3's previous hash" $
      (prevHash (fst (getLastBlock b3))) `shouldBe` (getBlockHash (fst (getLastBlock b2)))

    it "test b2's block header" $
      (isValidBlockHeader (fst (getLastBlock b2)) targetDifficulty) `shouldBe` True

    it "test b3's block header" $
      (isValidBlockHeader (fst (getLastBlock b3)) targetDifficulty) `shouldBe` True

    it "test fake b3 (proof is forged)" $
      (isValidBlockHeader (fst (getLastBlock b3_fake1)) targetDifficulty) `shouldBe` False

    it "test fake b3 (prevHash is forged)" $
      (isValidBlockHeader (fst (getLastBlock b3_fake2)) targetDifficulty) `shouldBe` False

    it "test fake b3 (timestamp is forged)" $
      (isValidBlockHeader (fst (getLastBlock b3_fake3)) targetDifficulty) `shouldBe` False

    it "test blockchain vaidity of b1" $
      (isValidChain b1) `shouldBe` True

    it "test blockchain vaidity of b2" $
      (isValidChain b2) `shouldBe` True

    it "test blockchain vaidity of b3" $
      (isValidChain b3) `shouldBe` True

    it "test blockchain vaidity of fake b3 (proof is forged) " $
      (isValidChain b3_fake1) `shouldBe` False

    it "test blockchain vaidity of fake b3 (prevHash is forged)" $
      (isValidChain b3_fake2) `shouldBe` False

    it "test blockchain vaidity of fake b3 (timestamp is forged)" $
      (isValidChain b3_fake3) `shouldBe` False