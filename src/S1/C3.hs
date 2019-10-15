{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module S1.C3 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Data.Char
import Data.Bifunctor
import Data.Word
import Data.Bits
import qualified S1.C1 as C
import qualified S1.C2 as C

-- Single-byte XOR cipher
-- The hex encoded string:
--
-- 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
-- ... has been XOR'd against a single character. Find the key, decrypt the message.
--
-- You can do this by hand. But don't: write code to do it for you.
--
-- How? Devise some method for "scoring" a piece of English plaintext. Character frequency
-- is a good metric. Evaluate each output and choose the one with the best score.
--
---------------------------------------------------------------------------
-- Achievement Unlocked                                                  --
-- You now have our permission to make "ETAOIN SHRDLU" jokes on Twitter. --
---------------------------------------------------------------------------

data C3Error
  = InvalidWord8BitList [Bool]
  | C2Err C.C2Error
  deriving (Show, Eq)

inputString :: String
inputString = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

twitterJoke :: String
twitterJoke = "ETAOIN SHRDLU"

singlebytexor
  :: ByteString
  -> Word8
  -> ByteString
singlebytexor inp b =
  BS.map (`xor` b) inp

getScore :: ByteString -> Int
getScore = B8.length . B8.filter (== ' ')

c3Complete
  :: String
  -> Either C3Error (Int, Char, ByteString)
c3Complete input = do
  hexstr <- hexStringToByteString input
  pure $ foldr (\b (best, key, bestStr) ->
                  let decoded = singlebytexor hexstr b
                      scored = getScore decoded
                  in
                    if scored > best then (scored, chr (fromIntegral b), decoded)
                    else (best, key, bestStr)
                    
                  ) (0, ' ', "") [0..255]

c3CharToHex :: Traversable t => t Char -> Either C3Error (t Word16)
c3CharToHex = first C2Err . traverse C.charToHex

hexStringToByteString :: String -> Either C3Error ByteString
hexStringToByteString inp = C.hexToBinary <$> c3CharToHex inp >>= fmap BS.pack . traverse makeWord8 . chunkBits 8

chunkBits :: Word -> [Bool] -> [[Bool]]
chunkBits nbits xs = go xs mempty
  where
    go [] acc = reverse acc
    go ys acc = let (bs, rest) = splitAt (fromIntegral nbits) ys in go rest (bs:acc)

makeWord8 :: [Bool] -> Either C3Error Word8
makeWord8 [a,b,c,d,e,f,g,h] =
  Right . sb 7 a . sb 6 b . sb 5 c . sb 4 d . sb 3 e . sb 2 f . sb 1 g . sb 0 h $ zeroBits
  where
    sb n True = (`setBit` n)
    sb _ False = id
makeWord8 bs =
  Left $ InvalidWord8BitList bs
