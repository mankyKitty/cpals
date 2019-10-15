{-# LANGUAGE TupleSections #-}
module S1.C4 where

import Control.Lens

import Data.Function

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import qualified Data.List as L

import Data.Char
import Data.Bifunctor
import Data.Word
import Data.Bits

import qualified S1.C1 as C
import qualified S1.C2 as C
import qualified S1.C3 as C

-- Detect single-character XOR
-- One of the 60-character strings in this file has been encrypted by single-character XOR.
--
-- Find it.
--
-- (Your code from #3 should help.)

findsinglebytexor :: [String] -> Either C.C3Error (Int, (Int, Char, ByteString))
findsinglebytexor = fmap (L.maximumBy (compare `on` (^._2._1))) . itraverse (\i -> fmap (i,) . C.c3Complete)

c4Complete :: IO ()
c4Complete = L.lines <$> readFile "files/4.txt" >>= print . findsinglebytexor
