module Main (main) where

import Data.Bits (Bits (shiftR), shiftL, (.&.), (.|.))
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.Function (on)
import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.PQueue.Min qualified as PQ
import Data.Word (Word64, Word8)
import Debug.Trace (trace, traceM)
import GHC.Base (Word8#)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

type Bit = Word8

data Node
  = Branch Node Node
  | Leaf Byte Int
  deriving (Eq, Show)

nodeCount :: Node -> Int
nodeCount (Leaf _ c) = c
nodeCount (Branch a b) = nodeCount a + nodeCount b

instance Ord Node where compare = compare `on` nodeCount

byteToBits :: Byte -> [Bit]
byteToBits b = [1 .&. (b `shiftR` i) | i <- [7, 6 .. 0]]

bitsToByte :: [Bit] -> Byte
bitsToByte bits =
  foldl' (.|.) 0 [fromIntegral (fromEnum b) `shiftL` i | (i, b) <- zip [7, 6 .. 0] bits]

fromBits :: [Bit] -> BS.Builder
fromBits bits = case splitAt 8 bits of
  ([], []) -> mempty
  (byte, []) -> BS.word8 $ bitsToByte byte
  (byte, rest) -> BS.word8 (bitsToByte byte) <> fromBits rest

serialize :: Node -> [Bit]
serialize (Leaf byte _) = 1 : byteToBits byte
serialize (Branch left right) = 0 : (serialize left <> serialize right)

data BitVec = BitVec {bytes :: ByteString, len :: Word64}

newBitVec :: ByteString -> BitVec
newBitVec bytes = BitVec{bytes, len = fromIntegral (BS.length bytes) * 8}

getBit :: BitVec -> Word64 -> Bit
getBit bitvec index = do
  let byteIndex = fromIntegral $ index `div` 8
  let bitIndex = fromIntegral $ index `mod` 8
  (BS.index bitvec.bytes byteIndex `shiftR` (7 - bitIndex)) .&. 1

getByte :: BitVec -> Word64 -> Word8
getByte bitvec index =
  (getBit bitvec index `shiftL` 7)
    .|. (getBit bitvec (index + 1) `shiftL` 6)
    .|. (getBit bitvec (index + 2) `shiftL` 5)
    .|. (getBit bitvec (index + 3) `shiftL` 4)
    .|. (getBit bitvec (index + 4) `shiftL` 3)
    .|. (getBit bitvec (index + 5) `shiftL` 2)
    .|. (getBit bitvec (index + 6) `shiftL` 1)
    .|. (getBit bitvec (index + 7) `shiftL` 0)

deserialize :: BitVec -> Word64 -> Maybe (Node, Word64)
deserialize bits cursor | cursor >= bits.len = Nothing
deserialize bits cursor = do
  case getBit bits cursor of
    0 -> do
      (left, cursor') <- deserialize bits (cursor + 1)
      (right, cursor'') <- deserialize bits cursor'
      Just (Branch left right, cursor'')
    1 -> do
      let byte = getByte bits (cursor + 1)
      Just (Leaf byte 0, cursor + 9)

type Byte = Word8

readTree :: ByteString -> Node
readTree input = buildTree queue
 where
  queue = PQ.fromList $ uncurry Leaf <$> M.toList occurences

  buildTree q
    | PQ.null q = undefined
  buildTree q = do
    let (left, q') = PQ.deleteFindMin q
    if PQ.null q'
      then left
      else do
        let (right, q'') = PQ.deleteFindMin q'
        buildTree $ PQ.insert (Branch left right) q''

  bytes = BS.unpack input

  occurences :: M.Map Byte Int
  occurences =
    foldl' (flip $ M.alter (Just . (+ 1) . fromMaybe 0)) M.empty bytes

encode :: Node -> ByteString -> ByteString
encode tree input =
  BS.toLazyByteString $
    mconcat
      [ BS.int64LE (BS.length input)
      , fromBits (serialize tree)
      , fromBits (concatMap translateByte (BS.unpack input))
      ]
 where
  translateByte :: Byte -> [Bit]
  translateByte byte = fromMaybe (error "translateByte failed") $ M.lookup byte lookupMap

  lookupMap :: M.Map Byte [Bit]
  lookupMap = go tree
   where
    go (Leaf byte _) = M.singleton byte [] :: M.Map Byte [Bit]
    go (Branch left right) = ((0 :) <$> go left) <> ((1 :) <$> go right)

decode :: ByteString -> Maybe ByteString
decode input | BS.length input < 8 = Nothing
decode input = do
  let (messageByteLen :: Word64) = foldl' (.|.) 0 [fromIntegral (BS.index input i) `shiftL` (fromIntegral i * 8) | i <- [0 .. 7]]
  let bits = newBitVec $ BS.drop 8 input
  (tree, body) <- deserialize bits 0
  let go cursor _ _ | cursor >= bits.len = []
      go _ _ i | i >= messageByteLen = []
      go cursor node i = case node of
        Branch left right -> do
          let bit = getBit bits cursor
          go (cursor + 1) (case bit of 0 -> left; 1 -> right) i
        Leaf byte _ ->
          byte:go cursor tree (i + 1)
  Just $ BS.pack $ go body tree 0

main :: IO ()
main = do
  -- let bits = newBitVec "Hallo"
  -- print $ [getBit bits i | i <- [8..15]]
  -- print $ getByte bits 8
  -- exitSuccess
  args <- getArgs
  if "encode" `elem` args
    then do
      tree <- readTree <$> BS.readFile (head (tail args))
      encoded <- encode tree <$> BS.readFile (head (tail args))
      BS.putStr encoded
    else BS.interact $ fromMaybe "" <$> decode
