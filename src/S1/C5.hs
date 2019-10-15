{-# LANGUAGE OverloadedStrings #-}
module S1.C5 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Data.Foldable
import Data.Word
import Data.Bits

import qualified S1.C1 as C
import qualified S1.C2 as C
import qualified S1.C3 as C

-- Implement repeating-key XOR
-- Here is the opening stanza of an important work of the English language:
--
-- Burning 'em, if you ain't quick and nimble
-- I go crazy when I hear a cymbal
--
-- Encrypt it, under the key "ICE", using repeating-key XOR.
--
-- In repeating-key XOR, you'll sequentially apply each byte of the key; the first byte of
-- plaintext will be XOR'd against I, the next C, the next E, then I again for the 4th
-- byte, and so on.
--
-- It should come out to:
--
-- 0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272
-- a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f
--
-- Encrypt a bunch of stuff using your repeating-key XOR function. Encrypt your mail. Encrypt
-- your password file. Your .sig file. Get a feel for it. I promise, we aren't wasting your
-- time with this.
data C5Error
  = InvalidLengthBitList
  deriving (Show, Eq)

input :: ByteString
input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

output :: ByteString
output = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

bytestringtohexstring :: ByteString -> Either C5Error ByteString
bytestringtohexstring = fmap B8.pack . (fmap . fmap) ((C.hexChars !!) . fromIntegral) . traverse mkHexWord8 . foldMap g . BS.unpack 
  where g = C.chunkBits 4 . C.padTo 8 False . C.convertBase (== 0x1) 2

mkHexWord8 :: [Bool] -> Either C5Error Word8
mkHexWord8 xs | length xs /= 4 = Left InvalidLengthBitList
              | otherwise = let [a,b,c,d] = xs
                            in pure . set 3 a . set 2 b . set 1 c . set 0 d $ zeroBits
  where
    set i True  = (`setBit` i)
    set _ False = id

c5Complete :: ByteString -> ByteString -> Either C5Error ByteString
c5Complete key = bytestringtohexstring . BS.pack . (\bs -> zipWith xor bs (repeatkey key)) . BS.unpack
  where repeatkey = fold . repeat . BS.unpack

c5Success :: Bool
c5Success = either (const False) (== output) $ c5Complete "ICE" input
