module JPEG
       ( -- * Types
         Jpeg(..)
       , jpeg
       , Channel
         -- * Colour Space Conversion
       , toY
       , toCb
       , toCr
         -- * IO
       , image
       ) where

import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Control.Monad ((>=>))
import qualified Data.ByteString as B
import           Data.Matrix as M
import           Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Word (Word8)
import           Lens.Micro

---

-- | A Jpeg has a height and width, and three colour channels from the
-- YCbCr colour space. Note that `Word8` is an 8-bit, unsigned integer.
data Jpeg = Jpeg { jWidth :: !Int
                 , jHeight :: !Int
                 , y' :: Channel Word8
                 , cb :: Channel Word8
                 , cr :: Channel Word8 } deriving (Eq,Show)

-- | A channel is a matrix of pixel values, not constrained to any
-- particular `Num` type.
type Channel a = Matrix a

-- | Will always yield an RGB `Image` when successful.
rgb :: DynamicImage -> Either String (Image PixelRGB8)
rgb (ImageYCbCr8 i) = Right $ convertImage i
rgb _ = Left "DynamicImage was not YCbCr."

-- | Convert a Vector of contiguous pixel values into their triples.
-- Vector size *must* be a multiple of 3!
trips :: Vector a -> Vector (a,a,a)
trips v | V.null v = V.empty
        | otherwise = (v V.! 0, v V.! 1, v V.! 2) `V.cons` trips (V.drop 3 v)

-- | Produce a Luminance Channel from RGB Channels.
-- https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
toY :: Channel Word8 -> Channel Word8 -> Channel Word8 -> Channel Word8
toY r g b = M.zipWith3 (\x y z -> x + y + z) r' g' b'
  where r' = M.map (round . (* (0.299 :: Double)) . fromIntegral) r
        g' = M.map (round . (* (0.587 :: Double)) . fromIntegral) g
        b' = M.map (round . (* (0.114 :: Double)) . fromIntegral) b

-- | Produce a Blue-Yellow Chrominance Channel from RGB Channels.
-- https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
toCb :: Channel Word8 -> Channel Word8 -> Channel Word8 -> Channel Word8
toCb r g b = M.zipWith3 (\x y z -> 128 - x - y + z) r' g' b'
  where r' = M.map (round . (* (0.168736 :: Double)) . fromIntegral) r
        g' = M.map (round . (* (0.331264 :: Double)) . fromIntegral) g
        b' = M.map (round . (* (0.5 :: Double)) . fromIntegral) b

-- | Produce a Red-Green Chrominance Channel from RGB Channels.
-- https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
toCr :: Channel Word8 -> Channel Word8 -> Channel Word8 -> Channel Word8
toCr r g b = M.zipWith3 (\x y z -> 128 + x - y - z) r' g' b'
  where r' = M.map (round . (* (0.5 :: Double)) . fromIntegral) r
        g' = M.map (round . (* (0.418688 :: Double)) . fromIntegral) g
        b' = M.map (round . (* (0.081312 :: Double)) . fromIntegral) b

-- | Create a `Jpeg`, with its values in the YCbCr colour space from a
-- JuicyPixels RGB `Image`.
jpeg :: Image PixelRGB8 -> Jpeg
jpeg (Image w h v) = Jpeg w' h' (toY c1 c2 c3) (toCb c1 c2 c3) (toCr c1 c2 c3)
  where w' = n8 w
        h' = n8 h
        n8 n = n - (n `mod` 8)
        m u = M.subMatrix (0,0) (w'-1,h'-1) $ M.fromVector (w,h) u
        (c1,c2,c3) = (V.unzip3 . trips $ VS.convert v) & each %~ m

-- | Read an image file into a JuicyPixels `Image` type.
image :: FilePath -> IO (Either String (Image PixelRGB8))
image fp = (decodeJpeg >=> rgb) <$> B.readFile fp
