module JPEG where

import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Control.Monad ((>=>))
import qualified Data.ByteString as B
import           Data.Matrix
import           Data.Vector as V
import qualified Data.Vector.Storable as VS

---

-- | A channel is a matrix of pixel values.
type Channel a = Matrix a

-- | Intermediate step in converting between `JuicyPixels` type
-- and the `Matrix` we'll use for compression.
data Intermediate = I Int Int (Vector Int, Vector Int, Vector Int)


-- | Will always yield an RGB `Image` when successful.
rgb :: DynamicImage -> Either String (Image PixelRGB8)
rgb (ImageYCbCr8 i) = Right $ convertImage i
rgb _ = Left "DynamicImage was not YCbCr."

-- | Keep every nth element of a given Vector.
every :: Int -> Vector a -> Vector a
every n v = go 1 v
  where go m u | V.null u = V.empty
               | m == n = V.head u `V.cons` go 1 (V.tail u)
               | otherwise = go (m+1) $ V.tail u

-- TODO: From here! `every` isn't right.
toInter :: Image PixelRGB8 -> Intermediate
toInter (Image w h v) = I w h (v1, v2, v3)
  where v1 = undefined
        v2 = undefined
        v3 = undefined

image :: IO (Either String (Image PixelRGB8))
image = (decodeJpeg >=> rgb) <$> B.readFile "kitten.jpg"

main :: IO ()
main = do
  i <- image
  case i of
    Left err -> putStrLn err
    Right img -> print . V.length . VS.convert $ imageData img

  
