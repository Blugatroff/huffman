module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.ST.Strict (runST)
import Data.Bits (Bits (shiftR), shiftL, (.&.), (.|.))
import Data.ByteString (StrictByteString)
import Data.ByteString qualified as BSS
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Internal qualified as BSSI
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Unsafe qualified as BSSU
import Data.Data (Proxy (Proxy))
import Data.Foldable (for_)
import Data.Function (on)
import Data.Functor (void)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.PQueue.Min qualified as PQ
import Data.Primitive (Prim, sizeOf)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector
import Data.Word (Word64, Word8)
import Debug.Trace (trace, traceM, traceShowId)
import Foreign (Storable (poke), plusPtr)
import GHC.Base (Int (I#), Int#, Word64#, Word8#, andI#, iShiftL#, iShiftRL#, orI#, (+#), (-#), (/=#), (<=#), (>=#))
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (IOMode (..), hClose, hFileSize, hPrint, openFile, stderr, withFile)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

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
  foldl' (.|.) 0 [b `shiftL` i | (i, b) <- zip [7, 6 .. 0] bits]

bitsToWord :: forall a. (Prim a) => (Num a) => (Bits a) => [Bit] -> a
bitsToWord bits =
  foldl' (.|.) 0 $ [fromIntegral b `shiftL` i | (i, b) <- zip (reverse [0 .. typeSize - 1]) bits]
 where
  typeSize = 8 * sizeOf (0 :: a)

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

readTree :: ByteString -> Maybe Node
readTree input = buildTree queue
 where
  queue = runST $ do
    occurences <- MVector.new 256
    for_ (BS.toChunks input) $ \chunk -> do
      let loop !i | i >= BSS.length chunk = pure ()
          loop i = do
            let index = fromIntegral $ BSS.index chunk i
            !count <- MVector.read occurences index
            MVector.write occurences index (count + 1)
            loop (i + 1)
      loop 0
    PQ.fromList . map (uncurry Leaf) . filter ((0 /=) . snd) . zip [0 ..] <$> MVector.foldr (:) [] occurences

  buildTree q
    | PQ.null q = Nothing
  buildTree q = do
    let (left, q') = PQ.deleteFindMin q
    if PQ.null q'
      then Just left
      else do
        let (right, q'') = PQ.deleteFindMin q'
        buildTree $ PQ.insert (Branch left right) q''

encode :: Node -> Word64 -> ByteString -> ByteString
encode tree inputLength input = header <> body''
 where
  header = BS.toLazyByteString (mconcat [BS.word64LE inputLength, serializedTree])
  body = BS.toLazyByteString (fromBits (concatMap translateByte (BS.unpack input)))
  serializedTree = fromBits (serialize tree)

  body' = BS.unfoldr nextByte (BS.unpack input, 0, 0)

  body'' = BS.fromChunks $ chunks input

  chunks :: ByteString -> [StrictByteString]
  chunks input = go (BS.toChunks input) 0# 0#
   where
    max :: Int# -> Int# -> Int#
    max a b = case a >=# b of
      0# -> b
      _ -> a
    go :: [StrictByteString] -> Int# -> Int# -> [StrictByteString]
    go [] _ 0# = []
    go [] bits n = BSS.singleton (fromIntegral (I# bits `shiftR` 56)) : go [] (bits `iShiftL#` 8#) (max 0# (n -# 8#))
    go (chunk : rest) bits nBits = do
      let (encodedChunk, nextEncodedChunks) = BSSI.unsafeCreateUptoN' (BSS.length chunk * 2) $ \ptr -> do
            let lopi :: Int -> Int -> Int# -> Int# -> IO (Int, [StrictByteString])
                lopi !outLen index bits nBits | index >= BSS.length chunk = pure (outLen, go rest bits nBits)
                lopi outLen index bits nBits = case nBits <=# 8# of
                  1# -> do
                    let byte = BSSU.unsafeIndex chunk index
                    let !(I# nextBits, I# nNextBits) = (Vector.!) lookupMap'' $ fromIntegral byte
                    lopi outLen (index + 1) (orI# bits (nextBits `iShiftRL#` nBits)) (nBits +# nNextBits)
                  _ -> do
                    let (byte :: Word8) = fromIntegral $ I# bits `shiftR` 56
                    poke (plusPtr ptr outLen) byte
                    lopi (outLen + 1) index (bits `iShiftL#` 8#) (nBits -# 8#)
            lopi 0 0 bits nBits
      encodedChunk : nextEncodedChunks

  nextByte :: ([Word8], Word64, Int) -> Maybe (Word8, ([Word8], Word64, Int))
  nextByte ([], _, 0) = Nothing
  nextByte ([], bits, n) = Just (fromIntegral (bits `shiftR` 56), ([], bits `shiftL` 8, max 0 (n - 8)))
  nextByte (rest, bits, nBits) | nBits >= 8 = Just (fromIntegral (bits `shiftR` 56), (rest, bits `shiftL` 8, nBits - 8))
  nextByte (byte : rest, bits, nBits) = do
    let (nextBits, nNextBits) = (Vector.!) lookupMap' $ fromIntegral byte
    nextByte (rest, bits .|. (nextBits `shiftR` nBits), nBits + nNextBits)

  translateByte :: Byte -> [Bit]
  translateByte byte = (Vector.!) lookupMap $ fromIntegral byte

  lookupMap'' :: Vector.Vector (Int, Int)
  lookupMap'' = Vector.generate 256 $ \i -> do
    let bits = fromMaybe undefined $ M.lookup (fromIntegral i) map
    (bitsToWord bits, length bits)
   where
    map = go tree
    go (Leaf byte _) = M.singleton byte [] :: M.Map Byte [Bit]
    go (Branch left right) = ((0 :) <$> go left) <> ((1 :) <$> go right)

  lookupMap' :: Vector.Vector (Word64, Int)
  lookupMap' = Vector.generate 256 $ \i -> do
    let bits = fromMaybe undefined $ M.lookup (fromIntegral i) map
    (bitsToWord bits, length bits)
   where
    map = go tree
    go (Leaf byte _) = M.singleton byte [] :: M.Map Byte [Bit]
    go (Branch left right) = ((0 :) <$> go left) <> ((1 :) <$> go right)

  lookupMap :: Vector.Vector [Bit]
  lookupMap = Vector.generate 256 $ \i -> fromMaybe undefined $ M.lookup (fromIntegral i) map
   where
    map = go tree
    go (Leaf byte _) = M.singleton byte [] :: M.Map Byte [Bit]
    go (Branch left right) = ((0 :) <$> go left) <> ((1 :) <$> go right)

decode :: ByteString -> Maybe ByteString
decode input | BS.length input < 8 = Nothing
decode input = do
  let (messageByteLen :: Word64) = foldl' (.|.) 0 [fromIntegral (BS.index input i) `shiftL` (fromIntegral i * 8) | i <- [0 .. 7]]
  (tree, bodyOffset) <- deserialize (newBitVec $ BS.drop 8 input) 0
  let body = concatMap byteToBits $ BS.unpack $ BS.drop (8 + fromIntegral (bodyOffset + 7) `div` 8) input
  let go [] _ _ = []
      go _ _ i | i >= messageByteLen = []
      go bits@(bit : rest) node i = case node of
        Branch left right -> do
          go rest (case bit of 0 -> left; 1 -> right) i
        Leaf byte _ ->
          byte : go bits tree (i + 1)
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
      let filepath = head $ tail args
      (tree, filesize) <- withFile filepath ReadMode $ \file -> do
        filesize :: Word64 <- fromIntegral <$> hFileSize file
        !tree <- readTree <$> BS.hGetContents file
        pure (tree, filesize)
      encoded <- encode (fromMaybe undefined tree) filesize <$> BS.readFile (head (tail args))
      BS.putStr encoded
    else -- print tree
      BS.interact $ fromMaybe "" <$> decode
