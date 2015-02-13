{-
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
- Find the amount of actual English that each mapping produces.
- The mapping with the highest accuracy should be the correct mapping.
-}

module Decrypt where

import           Control.Lens
import           Control.Monad
import           Data.List (sort,sortBy,nub)
import qualified Data.Map.Lazy as M
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Random
import           System.Random.Shuffle

---

type Distance  = Float
type Frequency = (Char,Float)

data Pair = P { pLetter :: Char
              , target  :: (Char,Distance) } deriving (Eq,Show,Ord)

targetL :: Lens' Pair (Char,Distance)
targetL f (P l t) = fmap (\t' -> P l t') $ f t

pairL :: Lens' Pair (Char,Char)
pairL f (P p (t,d)) = fmap (\(p',t') -> P p' (t',d)) $ f (p,t)

data Letter = L { lLetter   :: Char
                , potential :: [(Char,Distance)] } deriving (Eq,Show)

potentialL :: Lens' Letter [(Char,Distance)]
potentialL f (L l p) = fmap (\p' -> L l p') $ f p

-- | A list of Char-to-Char mappings, with the total Statistical Distance.
data Mapping = M { pairs    :: [Pair]
                 , distance :: Float } deriving (Eq,Show,Ord)

pairsL :: Lens' Mapping [Pair]
pairsL f (M p d) = fmap (\p' -> M p' d) $ f p

---

-- | Given a Set of English words, how much real English is present
-- in the text?
accuracy :: S.Set Text -> Text -> Float
accuracy s t = sco * tot
  where t'  = T.words t
        t'' = filter (`S.member` s) t'
        f w = 2 ^ (T.length w - 1)
        sco = sum . map f . nub $ t''
        tot = fil t'' / fil t'
        fil = fromIntegral . length

-- TODO: Make this return a `Decryption` type with Accuracy
-- and longest matched word included.
-- This is so that the result of this can be piped into
-- another version of the algorithm that guesses based on word hints.
-- (a la Colossus).
-- | Given a Mapping, tries to decrypt the cipher text.
decrypt :: Text -> Mapping -> Maybe Text
decrypt t m = fmap T.pack . mapM f . T.unpack . T.unwords . T.lines $ t
  where f ' ' = Just ' '
        f c   = c `M.lookup` m'
        m'    = M.fromList $ m ^.. pairsL . traverse . pairL

allMappings :: StdGen -> Int -> [Letter] -> [Pair] -> [Mapping]
allMappings g n ls as = allMs g [1..n] ls as

allMs :: StdGen -> [Int] -> [Letter] -> [Pair] -> [Mapping]
allMs _ [] _ _ = []
allMs g (_:ns) ls as = M (as ++ ps) d : allMs (snd $ next g) ns ls as
  where ls' = shuffle' ls len g
        ps  = fst $ foldl f ([],[]) ls'
        d   = sum $ ps ^.. traverse . targetL . _2
        len = length ls
        f (acc,u) l =
          let l' = l & potentialL %~ filter (flip notElem u . fst)
              cs = l' ^. potentialL . to (take 3)
              tf = floor (sqrt (fromIntegral $ length as)) + 1
              c  = head $ shuffle' cs (length cs) g
          in (P (lLetter l) c : acc, fst c : u)

-- `cs` and `rs` need the same length.
letters :: [Frequency] -> [Frequency] -> [Letter]
letters rs cs = map f cs
  where f (c,f1) = L c . sortBy p . map (_2 %~ (abs . (f1 -))) $ rs
        p (_,f1) (_,f2) = compare f1 f2

-- | Given assumptions, prunes those characters from the remaining
-- cipher letters, as well as the potential targets.
prune :: [Pair] -> [Frequency] -> [Frequency] -> ([Frequency],[Frequency])
prune ps rs cs = (rs',cs')
  where rs' = rs ^.. traverse . filtered (flip notElem ts . fst)
        cs' = cs ^.. traverse . filtered (flip notElem ls' . fst)
        ls' = map pLetter ps
        ts  = ps ^.. traverse . targetL . _1

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

-- Most common: e t a o i n s h r d l
-- Sum of top 09: 69.56%
-- Sum of top 11: 77.83%
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
