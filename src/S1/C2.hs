module S1.C2 where

-- Fixed XOR
-- Write a function that takes two equal-length buffers and produces their XOR combination.
--
-- If your function works properly, then when you feed it the string:
--
-- 1c0111001f010100061a024b53535009181c
-- ... after hex decoding, and when XOR'd against:
-- 686974207468652062756c6c277320657965
-- ... should produce:
-- 746865206b696420646f6e277420706c6179

import Control.Error

import Data.Word
import Data.Bits

import S1.C1 (hexValueToBinary, binaryToNewBase)

data C2Error
  = InvalidHexValue Char 
  | InputLengthsDontMatch Int Int
  deriving (Eq, Show)

charToHex :: Char -> Either C2Error Word16
charToHex x = note (InvalidHexValue x) . readMay $ "0x" <> [x]

hexChars :: [Char]
hexChars = "0123456789abcdef"

fixedXOR
  :: String
  -> String
  -> Either C2Error String
fixedXOR a b
  | length a /= length b = Left (InputLengthsDontMatch (length a) (length b))
  | otherwise = do
      hexA <- traverse g a
      hexB <- traverse g b
      pure $ (hexChars !!) . binaryToNewBase 2 <$> zipWith (zipWith xor) hexA hexB
        where
          g = fmap hexValueToBinary . charToHex

s1c2complete :: Bool
s1c2complete =
  (Right "746865206b696420646f6e277420706c6179")
  ==
  fixedXOR "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
