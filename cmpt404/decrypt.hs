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
- Find the top `k` Mappings which have the smallest total stat distance.
  One of them should be the correct mapping.
-}

module Decrypt where

import           Control.Lens
import           Control.Monad
import           Data.List (sort,sortBy)
import qualified Data.Map.Lazy as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Random
import           System.Random.Shuffle

---

type Distance  = Float
type Frequency = (Char,Float)

data Pair = P { pLetter :: Char
              , target  :: (Char,Distance) } deriving (Eq,Show)

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
                 , distance :: Float } deriving (Eq,Show)

pairsL :: Lens' Mapping [Pair]
pairsL f (M p d) = fmap (\p' -> M p' d) $ f p

---

j = do
  c  <- TIO.readFile "longCipher.txt"
  g  <- newStdGen
  let cs = textFreqs c
      (rs',cs') = prune assumptions realFreqs cs
      ls = letters rs' cs'
      ms = take 100 . sortBy p . allMappings g 5000 ls $ assumptions
  print cs
  print $ map distance ms
  print $ (head ms) ^.. pairsL . traverse . pairL
  return $ decrypt c $ head ms
  where p (M _ d1) (M _ d2) = compare d1 d2

decrypt :: Text -> Mapping -> Maybe Text
decrypt t m = fmap T.pack . mapM f . T.unpack . T.unwords . T.lines $ t
  where f ' ' = Just ' '
        f c   = c `M.lookup` m'
        m'    = M.fromList $ m ^.. pairsL . traverse . pairL

-- Most common: e t a o i n s h r d l
assumptions :: [Pair]
assumptions = [ P 'h' ('e',0)
              , P 'w' ('t',0)
              , P 'd' ('a',0)
              , P 'l' ('i',0)
              , P 'q' ('n',0)
              , P 'k' ('h',0)
              , P 'o' ('l',0) ]

{-}
assumptions = [ P 'h' ('e',0)
              , P 'd' ('a',0)
              , P 'k' ('h',0)                
              , P 'l' ('i',0)
              , P 's' ('n',0)
              , P 't' ('o',0)
              , P 'w' ('r',0)
              , P 'x' ('s',0)
              , P 'y' ('t',0) ]
              , P 'a' ('k',0)
              , P 'f' ('c',0)
              , P 'b' ('l',0)
              , P 'u' ('p',0)
              , P 'i' ('f',0)
              , P 'z' ('u',0)
              , P 'c' ('m',0) ] -}

allMappings :: StdGen -> Int -> [Letter] -> [Pair] -> [Mapping]
allMappings g n ls as = f g [1..n]
  where lsLen = length ls
        f _ [] = []
        f g' (_:ns) = 
          let ls' = shuffle' ls lsLen g'
              ps  = fst $ foldl allMappings' ([],[]) ls'
              d   = sum $ ps ^.. traverse . targetL . _2
          in M (as ++ ps) d : f (snd $ next g') ns

allMappings' :: ([Pair],[Char]) -> Letter -> ([Pair],[Char])
allMappings' (acc,u) l = (P (lLetter l) c : acc, fst c : u)
  where l' = l & potentialL %~ filter (flip notElem u . fst)
        c  = l' ^. potentialL . to head

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

-- e t a o i n s h r d l
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
