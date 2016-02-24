import Data.Matrix as M
import JPEG
import Lens.Micro
import System.Environment

---

-- | Read a given JPEG file, compress it, decompress it, report the error,
-- and save the image back out.
main :: IO ()
main = do
  (fp:_) <- getArgs
  i <- jpeg fp
  case i of
    Left e -> putStrLn e
    Right (Jpeg w h y cb cr) -> do
      putStrLn "Image read successfully."
      putStrLn "Compressing and decompressing again..."
      let y'  = compDecomp y
          cb' = compDecomp cb
          cr' = compDecomp cr
      putStrLn $ "Y' Error: " ++ show (err (w,h) y y')
      putStrLn $ "Cb Error: " ++ show (err (w,h) cb cb')
      putStrLn $ "Cr Error: " ++ show (err (w,h) cr cr')

-- | Compress and decompress each full JPEG channel.
compDecomp :: Chan a Int -> Chan a Int
compDecomp c = unblocks (blocks c & each . each %~ iso)

-- | The Compression-Decompression Isomorphism for a single 8x8 channel.
iso :: Chan a Int -> Chan a Int
iso = unshift . idct . unquantize q50 . quantize q50 . dct . shift

-- | Average error for a single 8x8 test channel.
exErr :: IO ()
exErr = do
  print ex
  let ex' = iso ex
      e = err (8,8) ex ex'
  print ex'
  putStrLn $ "Isomorphism Error: " ++ show e

-- | Calculate the error between to 8x8 channels.
err :: (Int,Int) -> Chan a Int -> Chan a Int -> Float
err (w,h) c1 c2 = (1/fromIntegral (w*h)) * fromIntegral (M.foldl (+) 0 errs)
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
