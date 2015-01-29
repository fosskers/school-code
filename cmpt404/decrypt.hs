{-
I want to choose some mapping, and calculate the sum of its positive
statistical distances.
In theory, the mapping with the smallest total distance will be
the correct one.

Need to allow for assumptions.

ALG:
- Remove assumptions from possible map targets.
- For each cipher letter, sort the remaining targets
  by their statistical distance from the cipher letter's
  frequency. This is a `Letter`.
- (Do n times): Scramble the list of Letters and fold.
  -- Ask each Letter who they want to be, provided that target
     hasn't been taken yet. Set each Letter's `target` in this way.
  -- Add assumption Letters to this list.
  -- Create a `Mapping` which also stores is total Stat. Distance.
- Find the top `k` Mappings which have the smallest total stat distance.
  One of them should be the correct mapping.
-}

module Decrypt where

import           Control.Applicative ((<$>))
import           Control.Lens
import           Control.Monad
import           Data.List (sort,sortBy)
import qualified Data.Map.Lazy as M
import           Data.Maybe (fromJust)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Random
import           System.Random.Shuffle

---

data Letter = L { letter    :: Char
                , target    :: Maybe Char
                , potential :: [(Char,Float)] } deriving (Eq,Show)

targetL :: Lens' Letter (Maybe Char)
targetL f (L l t p) = fmap (\t' -> L l t' p) $ f t

potentialL :: Lens' Letter [(Char,Float)]
potentialL f (L l t p) = fmap (\p' -> L l t p') $ f p

type Frequency = (Char,Float)

data Mapping = M Char Float Char Float deriving (Eq,Show)

---

---

-- | Given assumptions, prunes those characters from the remaining
-- cipher letters, as well as the potential targets.
prune :: [Letter] -> [Frequency] -> [Frequency] -> ([Frequency],[Frequency])
prune ls rs cs = (rs',cs')
  where rs' = rs ^.. traverse . filtered (flip notElem ts . fst)
        cs' = cs ^.. traverse . filtered (flip notElem ls' . fst)
        ls' = map letter ls
        ts  = ls ^.. traverse . targetL . _Just

-- `cs` and `rs` need the same length.
letters :: [Frequency] -> [Frequency] -> [Letter]
letters rs cs = map f cs
  where f (c,f1) = L c Nothing . sortBy p . map (_2 %~ (abs . (f1 -))) $ rs
        p (_,f1) (_,f2) = compare f1 f2

-- | At what frequency does each character appear in the cipher text?
textFreqs :: Text -> [Frequency]
textFreqs t = map f items
  where sorted  = sortT . mconcat . (T.lines >=> T.words) $ t
        grouped = M.fromList . map pair . T.group $ sorted
        pair l  = (T.head l, 100 * fil l / txtLen)
        txtLen  = fil sorted
        fil     = fromIntegral . T.length
        f i     = case M.lookup i grouped of
                   Nothing -> (i,0)  -- Char wasn't present in cipher.
                   Just fr -> (i,fr)

-- | All possible objects that can be mapped to and from.
items :: [Char]
items = ['a' .. 'z']

-- Borrowed from https://en.wikipedia.org/wiki/Letter_frequency
realFreqs :: [Frequency]
realFreqs = [ ('a',8.167)
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

-- | Sort a `Text` value.
sortT :: Text -> Text
sortT = T.pack . sort . T.unpack

-- | Sort by letter frequency. *DEAD*
--sortF :: [Frequency] -> [Frequency]
--sortF = sortBy (\(_,f1) (_,f2) -> compare f2 f1)

---

{-}
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
-}

--f = decrypt <$> c
g = textFreqs <$> c
--h = (`pairing` realFreqs) <$> g
i = shuffle' [1..10] 10 <$> newStdGen
c = TIO.readFile "cipher.txt"

j = do
  cs <- textFreqs <$> c
  let (rs',cs') = prune assumptions realFreqs cs
      ls = letters rs' cs'
  return ls

assumptions :: [Letter]
assumptions = [ L 'h' (Just 'e') []
              , L 'd' (Just 'a') [] ]

-- use `next` to get another generator from one you have
