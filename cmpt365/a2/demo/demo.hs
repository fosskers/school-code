import           Codec.Picture.Jpg
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Matrix as M
import           JPEG
import           Lens.Micro
import           System.Environment

---

-- | Read a given JPEG file, compress it, decompress it, report the error,
-- and save the image back out.
main :: IO ()
main = do
  (fp:_) <- getArgs
  i <- jpeg fp
  case i of
    Left e -> putStrLn e
    Right j@(Jpeg w h y cb cr) -> do
      putStrLn "Image read successfully."
      putStrLn $ "Image size: " ++ show (w,h)
      putStrLn "Compressing and decompressing again..."
      let y'  = compDecomp y
          cb' = compDecomp cb
          cr' = compDecomp cr
      putStrLn $ "Y' Error: " ++ show (err (w,h) y y')
      putStrLn $ "Cb Error: " ++ show (err (w,h) cb cb')
      putStrLn $ "Cr Error: " ++ show (err (w,h) cr cr')
      putStrLn "Writing out new Image..."
      let i' = toImage' $ Jpeg w h y' cb' cr'
      let i'' = toImage' j
      BL.writeFile ("new-" ++ fp) $ encodeJpegAtQuality 100 i'
      BL.writeFile ("orig-" ++ fp) $ encodeJpegAtQuality 100 i''
      putStrLn "Done."

-- | Q: Is there a problem with JuicyPixels output?
-- A: No. It's the type conversion! Without the `toImage' . toJ'`,
-- this outputs the same image it was given. With those, the image is
-- oddly skewed.
foo :: IO ()
foo = do
  f <- B.readFile "/home/colin/code/school-code/cmpt365/a2/small-kitten.jpeg"
  let f' = encodeJpegAtQuality 100 . toImage' . toJ' <$> (decodeJpeg f >>= ycbcr)
  case f' of
    Left _ -> putStrLn "Shit."
    Right f'' -> BL.writeFile "/home/colin/code/school-code/cmpt365/a2/test.jpeg" $ f''

-- | Compress and decompress a full JPEG channel.
compDecomp :: Chan a Int -> Chan a Int
compDecomp c = unblocks (blocks c & each . each %~ iso)

-- | The Compression-Decompression Isomorphism for a single 8x8 channel.
iso :: Chan a Int -> Chan a Int
iso = unshift . idct . unquantize q50 . quantize q50 . dct . shift

-- | Calculate the error between two channels.
err :: (Int,Int) -> Chan a Int -> Chan a Int -> Float
err (w,h) c1 c2 = (1/fromIntegral (w*h)) * fromIntegral (M.foldl (+) 0 errs)
  where errs = M.zipWith (\a b -> abs $ a - b) (_mat c1) (_mat c2)
