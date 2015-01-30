import           Control.Applicative ((<$>))
import           Data.List (sortBy)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Decrypt
import           System.Random

---

main :: IO (Maybe Float, Mapping, Maybe T.Text)
main = do
  c <- TIO.readFile "cipher.txt"
  s <- S.fromList . T.lines <$> TIO.readFile "top5000.txt"
  g <- newStdGen  
  let cs = textFreqs c
      (rs',cs') = prune assumptions realFreqs cs
      ls = letters rs' cs'
      ms = sortBy p1 . allMappings g 10000 ls $ assumptions
      as = sortBy p2 $ map f ms
      f m = let dt = decrypt c m in (accuracy s <$> dt,m,dt)
  return $ head as  --take 10 as ^.. traverse . _1
  where p1 (M _ d1) (M _ d2) = compare d1 d2
        p2 (a1,_,_) (a2,_,_) = compare a2 a1

-- Most common: e t a o i n s h r d l
-- Sum of top 09: 69.56%
-- Sum of top 11: 77.83%
assumptions :: [Pair]
--assumptions = []

-- OpenGL
{-}
assumptions = [ P 'w' ('t',0)
              , P 'd' ('a',0)
              , P 'l' ('i',0)
              , P 'q' ('n',0)
              , P 'k' ('h',0)
              , P 'o' ('l',0) ] -}

-- TODO: Don't need to tell it the obvious ones. Need to tell it
-- the hard to distinguish ones! Find out which ones these are.
-- HOBBIT
assumptions = [ P 'l' ('i',0)
              , P 'k' ('h',0)                
              , P 's' ('n',0)
              , P 'w' ('r',0)
              , P 'x' ('s',0)
              , P 'b' ('l',0)
              , P 'i' ('f',0) ] {-}
              , P 'y' ('t',0) ]
              , P 'd' ('a',0)
              , P 't' ('o',0)

              , P 'a' ('k',0)
              , P 'f' ('c',0)

              , P 'u' ('p',0)

              , P 'z' ('u',0)
              , P 'c' ('m',0) ] -}
