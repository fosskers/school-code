module JPEG where

import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Control.Monad ((>=>))
import qualified Data.ByteString as B
import           Data.Matrix as M
import           Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Word (Word8)

---

-- | A Jpeg has a height and width, and three colour channels from the
-- YCbCr colour space.
data Jpeg = Jpeg { jWidth :: !Int
                 , jHeight :: !Int
                 , y' :: Channel Word8
                 , cb :: Channel Word8
                 , cr :: Channel Word8 } deriving (Eq,Show)

-- | A channel is a matrix of pixel values.
type Channel a = Matrix a

-- | A contiguous Vector of pixel values for Channel, with no associated
-- width or height values.
type ChanVec = Vector Word8

-- | Will always yield an RGB `Image` when successful.
rgb :: DynamicImage -> Either String (Image PixelRGB8)
rgb (ImageYCbCr8 i) = Right $ convertImage i
rgb _ = Left "DynamicImage was not YCbCr."

-- | Convert a Vector of contiguous pixel values into their triples.
-- Vector size *must* be a multiple of 3!
trips :: Vector a -> Vector (a,a,a)
trips v | V.null v = V.empty
        | otherwise = (v V.! 0, v V.! 1, v V.! 2) `V.cons` trips (V.drop 3 v)

-- TODO: Colour space conversion!
-- | Create a `Jpeg`, with its values in the YCbCr colour space from a
-- JuicyPixels RGB `Image`.
jpeg :: Image PixelRGB8 -> Jpeg
jpeg (Image w h v) = Jpeg w' h' (m c1) (m c2) (m c3)
  where w' = n8 w
        h' = n8 h
        n8 n = n - (n `mod` 8)
        m u = M.subMatrix (0,0) (w'-1,h'-1) $ M.fromVector (w,h) u
        (c1,c2,c3) = V.unzip3 . trips $ VS.convert v

-- | Read an image file into a JuicyPixels `Image` type.
image :: FilePath -> IO (Either String (Image PixelRGB8))
image fp = (decodeJpeg >=> rgb) <$> B.readFile fp
