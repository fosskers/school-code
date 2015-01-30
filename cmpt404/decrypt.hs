{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Control.Lens
import           Data.List (sortBy)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Decrypt
import           System.Random

---

main :: IO ()
main = do
  c <- TIO.readFile "cipher.txt"
  s <- S.union properNouns . S.fromList . T.lines <$> TIO.readFile "top5000.txt"
  g <- newStdGen 
  let cs = textFreqs c
      (rs',cs') = prune assumptions realFreqs cs
      ls = letters rs' cs'
      ms = sortBy p1 . allMappings g 10000 ls $ assumptions
      as = sortBy p2 $ map f ms
      f m = let dt = decrypt c m in (accuracy s <$> dt,dt)
  print $ head as --take 20 as ^.. traverse . _1
  where p1 (M _ d1) (M _ d2) = compare d1 d2
        p2 (a1,_) (a2,_) = compare a2 a1

-- Max score: 2315
properNouns :: S.Set T.Text
properNouns = S.fromList ["dori","nori","ori","oin","gloin","fili","kili",
                          "bombur","bofur","dwalin","balin","thorin",
                          "oakenshield","bilbo"]

-- Most common: e t a o i n s h r d l
-- Sum of top 09: 69.56%
-- Sum of top 11: 77.83%
assumptions :: [Pair]
--assumptions = []

-- OpenGL
{-
assumptions = [ P 'w' ('t',0)
              , P 'd' ('a',0)
              , P 'l' ('i',0)
              , P 'q' ('n',0)
              , P 'k' ('h',0)
              , P 'o' ('l',0) ] -}

-- HOBBIT
assumptions = [ P 'i' ('f',0)  -- 2.228
              , P 'f' ('c',0)  -- 2.782
              , P 'x' ('s',0)  -- 6.327
              , P 's' ('n',0)  -- 6.749
              , P 'l' ('i',0)  -- 6.966
              ] {-
              , P 'b' ('l',0)  -- 4.024
              , P 'k' ('h',0)  -- 6.094
              , P 'w' ('r',0)  -- 5.987
              , P 'c' ('m',0)  -- 2.406
              , P 'a' ('k',0)  -- 0.772
              , P 'u' ('p',0)  -- 1.929


              , P 'y' ('t',0)  -- 9.056

              

              , P 't' ('o',0)  -- 7.507

              , P 'z' ('u',0)  -- 2.758

-}
