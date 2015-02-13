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
  print $ work c s g

conner :: IO ()
conner = do
  s <- S.union properNouns . S.fromList . T.lines <$> TIO.readFile "top5000.txt"
  g <- newStdGen
  print $ work perfect s g

--work :: T.Text -> S.Set T.Text -> StdGen -> (Maybe Float,Maybe T.Text)
work c s g = head as --take 30 $ as ^.. traverse . _1
  where cs = textFreqs c
        (rs',cs') = prune assumptions realFreqs cs
        ls = letters rs' cs'
        ms = sortBy p1 . allMappings g 10000 ls $ assumptions
        as = sortBy p2 $ map f ms
        f m = let dt = decrypt c m in (accuracy s <$> dt,dt)
        p1 (M _ d1) (M _ d2) = compare d1 d2
        p2 (a1,_) (a2,_) = compare a2 a1

oshii :: T.Text
oshii = "dead silence fell in the biddle of a word omt went all lights the fire leaued mu in plack sbokes ashes and cinders were in the eyes of the dwarjes and the wood was filled again with their clabomr and their cries pilpo fomnd hibself rmnning romnd and romnd as he thomght and calling and calling dori nori ori oin gloin fili kili pobpmr pofmr dwalin palin thorin oakenshield while ueoule he comld not see or feel were doing the sabe all romnd hib with an occasional pilpo thrown in pmt the cries of the others got steadily fmrther and fainter and thomgh after a while it seebed to hib they changed to yells and cries for helu in the distance all noise at last died right away and he was left alone in cobulete silence and darkness"

perfect :: T.Text
perfect = "dead silence fell in the middle of a word out went all lights the fire leaped up in black smokes ashes and cinders were in the eyes of the dwarves and the wood was filled again with their clamour and their cries bilbo found himself running round and round as he thought and calling and calling dori nori ori oin gloin fili kili bombur bofur dwalin balin thorin oakenshield while people he could not see or feel were doing the same all round him with an occasional bilbo thrown in but the cries of the others got steadily further and fainter and though after a while it seemed to him they changed to yells and cries for help in the distance all noise at last died right away and he was left alone in complete silence and darkness"

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
assumptions = [ P 'l' ('i',0)  -- 6.966
              , P 's' ('n',0)  -- 6.749
              , P 'y' ('t',0)  -- 9.056
              , P 'x' ('s',0)  -- 6.327
              , P 'i' ('f',0)  -- 2.228
              , P 'f' ('c',0)  -- 2.782
              , P 't' ('o',0)  -- 7.507
              , P 'c' ('m',0)
              , P 'z' ('u',0)  -- 2.758
              ]

