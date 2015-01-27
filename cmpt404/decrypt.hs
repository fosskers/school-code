{-
I want to choose some mapping, and calculate the sum of its positive
statistical distances.
In theory, the mapping with the smallest total distance will be
the correct one.

Need to allow for assumptions.
-}

module Decrypt where

import           Control.Monad
import           Data.List (sort,sortBy)
import qualified Data.Map.Lazy as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

---

data Mapping = M Char Float Char Float deriving (Eq,Show)

type Frequency = (Char,Float)

---

decrypt :: Text -> Maybe Text
decrypt t = fmap T.pack . mapM m . T.unpack . T.unwords . T.lines $ t
  where m = mapping (textFreqs t) realFreqs

distance :: [Mapping] -> Float
distance = sum . map distance'
  where distance' (M _ f1 _ f2) = if f1 - f2 > 0 then f1 - f2 else 0

-- | Maps Chars from one distribution to another.
mapping :: [Frequency] -> [Frequency] -> Char -> Maybe Char
mapping _ _ ' ' = Just ' '
mapping f1 f2 c = M.lookup c $ pairing f1 f2

pairing :: [Frequency] -> [Frequency] -> M.Map Char Char
pairing f1 = M.fromList . zipWith (\(c1,_) (c2,_) -> (c1,c2)) f1

-- Borrowed from https://en.wikipedia.org/wiki/Letter_frequency
realFreqs :: [Frequency]
realFreqs =  sortF [ ('a',8.167)
                   , ('b',1.492)
                   , ('c',2.782)
                   , ('d',4.253)
                   , ('e',12.702)
                   , ('f',2.228)
                   , ('g',2.015)
                   , ('h',6.094)
                   , ('i',6.966)
                   , ('j',0.153)
                   , ('k',0.772)
                   , ('l',4.025)
                   , ('m',2.406)
                   , ('n',6.749)
                   , ('o',7.507)
                   , ('p',1.929)
                   , ('q',0.095)
                   , ('r',5.987)
                   , ('s',6.327)
                   , ('t',9.056)
                   , ('u',2.758)
                   , ('v',0.978)
                   , ('w',2.360)
                   , ('x',0.150)
                   , ('y',1.974)
                   , ('z',0.074) ]
            
textFreqs :: Text -> [Frequency]
textFreqs t = sortF . map pair . T.group $ sorted
  where sorted  = sortT . mconcat . (T.lines >=> T.words) $ t
        txtLen  = fil sorted
        fil     = fromIntegral . T.length
        pair t' = (T.head t', 100 * fil t' / txtLen)

-- | Sort a `Text` value.
sortT :: Text -> Text
sortT = T.pack . sort . T.unpack

-- | Sort by letter frequency.
sortF :: [Frequency] -> [Frequency]
sortF = sortBy (\(_,f1) (_,f2) -> compare f2 f1)

f = fmap decrypt $ TIO.readFile "cipher.txt"
g = fmap textFreqs $ TIO.readFile "cipher.txt"
h = fmap (`pairing` realFreqs) g
