import Data.Matrix as M
import JPEG
import System.Environment

---

main :: IO ()
main = do
  (fp:_) <- getArgs
  i <- jpeg fp
  case i of
    Left err -> putStrLn err
    Right img -> do
      print $ _width img
      print $ _height img
      print $ (_mat $ _y' img) M.! (0,0)

-- | Average error for a single 8x8 test channel.
iso :: IO ()
iso = do
  print ex
  let ex' = unshift . idct . unquantize q50 . quantize q50 . dct $ shift ex
      e = err ex ex'
  print ex'
  putStrLn $ "Isomorphism Error: " ++ show e

-- | Calculate the error between to 8x8 channels.
err :: Chan a Int -> Chan a Int -> Float
err c1 c2 = (1/64) * fromIntegral (M.foldl (+) 0 errs)
  where errs = M.zipWith (\a b -> abs $ a - b) (_mat c1) (_mat c2)

ex :: Chan a Int
ex = Chan $ chan [ 52,55,61,66,70,61,64,73
                 , 63,59,55,90,109,85,69,72
                 , 62,59,68,113,144,104,66,73
                 , 63,58,71,122,154,106,70,69
                 , 67,61,68,104,126,88,68,70
                 , 79,65,60,70,77,68,58,75
                 , 85,71,64,59,55,61,65,83
                 , 87,79,69,68,65,76,78,94 ]
