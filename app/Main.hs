module Main (main) where

import Data.ByteString (StrictByteString)
import Data.ByteString qualified as BSS
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Internal qualified as BSSI
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Unsafe qualified as BSSU

import Data.Bits (Bits (complement, shiftR), shiftL, (.&.), (.|.))
import Data.Foldable (for_)
import Data.Function (on)
import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Primitive (Prim, sizeOf)
import Data.Traversable (for)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector
import Data.Word (Word16, Word32, Word64, Word8)

import Foreign (Storable (poke, pokeElemOff), plusPtr)

import GHC.Base (Int (I#), Int#, iShiftL#, iShiftRL#, orI#, (+#), (-#), (<=#), (>=#))

import Control.Applicative ((<|>))
import Control.Monad.ST.Strict (runST)

import System.Environment (getArgs)
import System.IO (Handle, IOMode (..), hFileSize, hPutStrLn, stderr, stdin, stdout, withFile)

data Node = Branch Node Node | Leaf Word8 Int
  deriving (Eq, Show)

nodeCount :: Node -> Int
nodeCount (Leaf _ c) = c
nodeCount (Branch a b) = nodeCount a + nodeCount b

instance Ord Node where compare = compare `on` nodeCount

type Bit = Word8

intoBits :: forall a. (Integral a, Prim a, Bits a) => a -> [Bit]
intoBits a = [fromIntegral (1 .&. (a `shiftR` (sizeOf (0 :: a) * 8 - i))) | i <- [1 .. sizeOf (0 :: a) * 8]]

assembleBits :: forall a. (Prim a, Num a, Bits a) => [Bit] -> a
assembleBits bits =
  foldl' (.|.) 0 $ [fromIntegral b `shiftL` i | (i, b) <- zip (reverse [0 .. 8 * sizeOf (0 :: a) - 1]) bits]

fromBits :: [Bit] -> BS.Builder
fromBits bits = case splitAt 8 bits of
  ([], []) -> mempty
  (byte, []) -> BS.word8 $ assembleBits byte
  (byte, rest) -> BS.word8 (assembleBits byte) <> fromBits rest

serialize :: Node -> [Bit]
serialize (Leaf byte _) = 1 : intoBits byte
serialize (Branch left right) = 0 : (serialize left <> serialize right)

newtype BitVec = BitVec {bytes :: ByteString}

getBit :: BitVec -> Word64 -> Bit
getBit bitvec index = do
  let byteIndex = fromIntegral $ index `div` 8
  let bitIndex = fromIntegral $ index `mod` 8
  (BS.index bitvec.bytes byteIndex `shiftR` (7 - bitIndex)) .&. 1

getByte :: BitVec -> Word64 -> Word8
getByte bitvec index = foldl' (.|.) 0 $ [getBit bitvec (index + i) `shiftL` (7 - fromIntegral i) | i <- [0 .. 7]]

deserialize :: BitVec -> Word64 -> Maybe (Node, Word64)
deserialize bits cursor = case getBit bits cursor of
  0 -> do
    (left, cursor') <- deserialize bits (cursor + 1)
    (right, cursor'') <- deserialize bits cursor'
    Just (Branch left right, cursor'')
  _ -> do
    let byte = getByte bits (cursor + 1)
    Just (Leaf byte 0, cursor + 9)

queueInsert :: (Ord a) => a -> [a] -> [a]
queueInsert v [] = [v]
queueInsert v (x : xs) | x < v = x : queueInsert v xs
queueInsert v (x : xs) = v : x : xs

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
    map (uncurry Leaf) . filter ((0 /=) . snd) . zip [0 ..] <$> MVector.foldr (:) [] occurences

  buildTree [] = Nothing
  buildTree [left] = Just left
  buildTree (left : right : q) = buildTree $ queueInsert (Branch left right) q

encode :: Node -> Word64 -> ByteString -> ByteString
encode tree inputLength input = header <> body
 where
  header = BS.toLazyByteString (mconcat [BS.word64LE inputLength, serializedTree])
  serializedTree = fromBits (serialize tree)
  body = BS.fromChunks $ chunks input

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
            let loop :: Int -> Int -> Int# -> Int# -> IO (Int, [StrictByteString])
                loop !outLen index bits nBits | index >= BSS.length chunk = pure (outLen, go rest bits nBits)
                loop outLen index bits nBits = case nBits <=# 8# of
                  1# -> do
                    let byte = BSSU.unsafeIndex chunk index
                    let !(I# nextBits, I# nNextBits) = (Vector.!) lookupMap $ fromIntegral byte
                    loop outLen (index + 1) (orI# bits (nextBits `iShiftRL#` nBits)) (nBits +# nNextBits)
                  _ -> do
                    let (byte :: Word8) = fromIntegral $ I# bits `shiftR` 56
                    poke (plusPtr ptr outLen) byte
                    loop (outLen + 1) index (bits `iShiftL#` 8#) (nBits -# 8#)
            loop 0 0 bits nBits
      encodedChunk : nextEncodedChunks

  lookupMap :: Vector.Vector (Int, Int)
  lookupMap = Vector.generate 256 $ \i -> do
    let go (Leaf byte _) = M.singleton byte [] :: M.Map Word8 [Bit]
        go (Branch left right) = ((0 :) <$> go left) <> ((1 :) <$> go right)
    let map = go tree
    let bits = fromMaybe [] $ M.lookup (fromIntegral i) map
    (assembleBits bits, length bits)

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap _ [] = Nothing
findMap f (x : xs) = f x <|> findMap f xs

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x : xs) = [x] : ((x :) <$> prefixes xs)

decode :: ByteString -> Maybe ByteString
decode input | BS.length (BS.take 8 input) /= 8 = Nothing
decode input = do
  (tree, bodyOffset) <- deserialize (BitVec $ BS.drop 8 input) 0

  let (messageByteLen :: Word64) = foldl' (.|.) 0 $ do
        [fromIntegral (BS.index input i) `shiftL` (fromIntegral i * 8) | i <- [0 .. 7]]

      body = BS.drop (8 + fromIntegral (bodyOffset + 7) `div` 8) input

      treeToMap (Leaf byte _) = M.singleton byte [] :: M.Map Word8 [Bit]
      treeToMap (Branch left right) = ((0 :) <$> treeToMap left) <> ((1 :) <$> treeToMap right)

      translationMap = treeToMap tree :: M.Map Word8 [Bit]

      longestRun :: Int = maximum (length . snd <$> M.toList translationMap)

      deTranslationMap :: M.Map [Bit] Word8
      deTranslationMap = M.fromList $ map (\(a, b) -> (b, a)) $ M.toList translationMap

      makeTableEntry :: Word16 -> (Word8, Int)
      makeTableEntry byte = fromMaybe undefined $ do
        let bits = drop (16 - longestRun) $ intoBits byte
        findMap (\prefix -> (,length prefix) <$> M.lookup prefix deTranslationMap) $ prefixes bits

      table = Vector.fromList $ map makeTableEntry [0 .. (2 ^ longestRun) - 1]
      (tableKeyMask :: Word32) = complement (1 `shiftL` (32 - longestRun) - 1)

      lookupInTable :: Word32 -> (Word8, Int)
      lookupInTable w = (Vector.!) table (fromIntegral (tableKeyMask .&. w) `shiftR` (32 - longestRun))

      go :: [StrictByteString] -> Word32 -> Int -> Word64 -> [StrictByteString]
      go _ !_ !_ !i | i >= messageByteLen = []
      go [] _ ws _ | ws < 0 = error "ws < 0"
      go [] _ 0 _ = []
      go [] w ws i = do
        let (decodedByte, choppedSize) = lookupInTable w
        BSS.singleton decodedByte : go [] (w `shiftL` choppedSize) (ws - choppedSize) (i + 1)
      go (chunk : chunks) w ws i = decodedChunk : decodedChunks
       where
        (decodedChunk, decodedChunks) = BSSI.unsafeCreateUptoN' (BSS.length chunk * 8) $ \ptr -> do
          let loop :: Word64 -> Word32 -> Int -> Int -> IO (Int, [StrictByteString])
              loop !outLen !_ !ws !_ | i + outLen >= messageByteLen = do
                pure (fromIntegral outLen, go chunks w ws (i + outLen))
              loop outLen w ws chunkI | chunkI >= BSS.length chunk = pure $ case ws of
                0 -> (fromIntegral outLen, go chunks w ws (i + outLen))
                ws | ws < 0 -> error "ws < 0 !"
                ws -> (fromIntegral outLen, go chunks w ws (i + outLen))
              loop outLen w ws chunkI | ws >= longestRun = do
                let (decodedByte, choppedSize) = lookupInTable w
                pokeElemOff ptr (fromIntegral outLen) decodedByte
                loop (outLen + 1) (w `shiftL` choppedSize) (ws - choppedSize) chunkI
              loop outLen w ws chunkI = do
                let byte = BSS.index chunk chunkI
                loop outLen (w .|. (fromIntegral byte `shiftL` (32 - 8 - ws))) (ws + 8) (chunkI + 1)
          loop 0 w ws 0
  Just $ BS.fromChunks $ go (BS.toChunks body) 0 0 0

hPutStrMeasureSize :: Handle -> ByteString -> IO Int
hPutStrMeasureSize h bs =
  fmap sum $ for (BS.toChunks bs) $ \chunk -> do
    BSS.hPutStr h chunk
    pure (BSS.length chunk)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["encode", filepath] -> do
      (tree, filesize) <- withFile filepath ReadMode $ \file -> do
        filesize :: Word64 <- fromIntegral <$> hFileSize file
        !tree <- readTree <$> BS.hGetContents file
        pure (tree, filesize)
      encoded <- encode (fromMaybe undefined tree) filesize <$> BS.readFile (head (tail args))
      encodedSize <- hPutStrMeasureSize stdout encoded
      hPutStrLn stderr $ "original size:   " <> show filesize
      hPutStrLn stderr $ "compressed size: " <> show encodedSize
      let ratio :: Double = fromIntegral (round ((fromIntegral encodedSize * 1000 :: Double) / fromIntegral filesize) :: Integer) / 10
      hPutStrLn stderr $ "ratio:           " <> show ratio <> "%"
    ["decode"] -> do
      input <- BS.hGetContents stdin
      case decode input of
        Nothing -> hPutStrLn stderr "Failed to decode"
        Just decoded -> BS.hPutStr stdout decoded
    _ -> do
      hPutStrLn stderr $ "unexpected arguments: " <> show args
      hPutStrLn stderr "available commands:"
      hPutStrLn stderr "    encode <file>"
      hPutStrLn stderr "    decode"
