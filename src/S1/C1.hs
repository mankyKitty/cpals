{-# LANGUAGE OverloadedStrings #-}
-- | Cryptopals challenge, set 1, challenge 1
-- Convert hex to base64
module S1.C1 where

import Data.Word
import qualified Data.List as L

-- This
-- testInput = "49276d206b696c6c96e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
testInput :: [Word16]
testInput = [0x4, 0x9, 0x2, 0x7, 0x6, 0xd, 0x2, 0x0, 0x6, 0xb, 0x6, 0x9, 0x6, 0xc, 0x6, 0xc, 0x6, 0x9, 0x6, 0xe, 0x6, 0x7, 0x2, 0x0, 0x7, 0x9, 0x6, 0xf, 0x7, 0x5, 0x7, 0x2, 0x2, 0x0, 0x6, 0x2, 0x7, 0x2, 0x6, 0x1, 0x6, 0x9, 0x6, 0xe, 0x2, 0x0, 0x6, 0xc, 0x6, 0x9, 0x6, 0xb, 0x6, 0x5, 0x2, 0x0, 0x6, 0x1, 0x2, 0x0, 0x7, 0x0, 0x6, 0xf, 0x6, 0x9, 0x7, 0x3, 0x6, 0xf, 0x6, 0xe, 0x6, 0xf, 0x7, 0x5, 0x7, 0x3, 0x2, 0x0, 0x6, 0xd, 0x7, 0x5, 0x7, 0x3, 0x6, 0x8, 0x7, 0x2, 0x6, 0xf, 0x6, 0xf, 0x6, 0xd]

-- should produce this
testOutput :: [Char]
testOutput = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

s1c1complete :: Bool
s1c1complete = testOutput == hextobase64 testInput

hextobase64 :: [Word16] -> [Char]
hextobase64 = fmap ((b64 !!) . fromIntegral) . binaryToBase64 . hexToBinary

b64 :: [Char]
b64 = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "+/"

b64Padding :: Char
b64Padding = '='

hexValueToBinary :: Word16 -> [Bool]
hexValueToBinary = padTo 4 False . convertBase (== 0x1) 2

-- First idea.. hex to binary, binary to base64
hexToBinary :: [Word16] -> [Bool]
hexToBinary = foldMap hexValueToBinary

binaryToBase64 :: [Bool] -> [Word8]
binaryToBase64 xs = binaryToNewBase 2 <$> sixbitss xs mempty
  where
    sixbitss [] acc = reverse acc
    sixbitss ys acc = let (six, rest) = splitAt 6 ys in sixbitss rest (six:acc)

binaryToNewBase :: Integral a => a -> [Bool] -> a
binaryToNewBase newBase xs = sum $ zipWith g powers xs
  where
    g e True  = newBase ^ e
    g _ False = 0

    powers = [(len - 1), (len - 2) .. 0]
    len = length xs

padTo :: Int -> a -> [a] -> [a]
padTo n a xs = let x = length xs in L.replicate (n - x) a <> xs

convertBase
  :: Integral a
  => (a -> b)
  -> a
  -> a
  -> [b]
convertBase asBase base num = go num mempty
  where
    go n acc =
      let
        (q, r) = (quot n base, rem n base)
      in
        if q == 0 then asBase r : acc
        else go q $ asBase r : acc
