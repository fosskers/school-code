{-# LANGUAGE ViewPatterns #-}

module JPEG
       ( -- * Types
         Jpeg(..)
       , jpeg
       , Chan
       , Y, Cb, Cr, R, G, B
         -- * Colour Space Conversion
         -- ** RGB to YCbCr
       , toY
       , toCb
       , toCr
         -- ** YCbCr to RGB
       , toR
       , toG
       , toB
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
data Jpeg = Jpeg { _width :: !Int
                 , _height :: !Int
                 , _y' :: Chan Y  Word8
                 , _cb :: Chan Cb Word8
                 , _cr :: Chan Cr Word8 } deriving (Eq,Show)

-- | The Y' Channel from YCbCr.
data Y

-- | The Cb Channel from YCbCr.
data Cb

-- | The Cr Channel from YCbCr.
data Cr

-- | The R Channel from RGB.
data R

-- | The G Channel from RGB.
data G

-- | The B Channel from RGB.
data B

-- | A channel is a matrix of pixel values, not constrained to any
-- particular `Num` type.
type Chan c a = Matrix a

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
toY :: Chan R Word8 -> Chan G Word8 -> Chan B Word8 -> Chan Y Word8
toY r g b = M.zipWith3 (\x y z -> x + y + z) r' g' b'
  where r' = M.map (round . (* (0.299 :: Double)) . fromIntegral) r
        g' = M.map (round . (* (0.587 :: Double)) . fromIntegral) g
        b' = M.map (round . (* (0.114 :: Double)) . fromIntegral) b

-- | Produce a Blue-Yellow Chrominance Channel from RGB Channels.
-- https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
toCb :: Chan R Word8 -> Chan G Word8 -> Chan B Word8 -> Chan Cb Word8
toCb r g b = M.zipWith3 (\x y z -> 128 - x - y + z) r' g' b'
  where r' = M.map (round . (* (0.168736 :: Double)) . fromIntegral) r
        g' = M.map (round . (* (0.331264 :: Double)) . fromIntegral) g
        b' = M.map (round . (* (0.5 :: Double)) . fromIntegral) b

-- | Produce a Red-Green Chrominance Channel from RGB Channels.
-- https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
toCr :: Chan R Word8 -> Chan G Word8 -> Chan B Word8 -> Chan Cr Word8
toCr r g b = M.zipWith3 (\x y z -> 128 + x - y - z) r' g' b'
  where r' = M.map (round . (* (0.5 :: Double)) . fromIntegral) r
        g' = M.map (round . (* (0.418688 :: Double)) . fromIntegral) g
        b' = M.map (round . (* (0.081312 :: Double)) . fromIntegral) b

-- TODO: Worry about overflow errors here...
toR :: Chan Y Word8 -> Chan Cr Word8 -> Chan R Word8
toR y cr = M.zipWith (+) y cr'
  where cr' = M.map (\(fromIntegral -> n) -> round $ 1.402 * (n - 128)) cr

toG :: Chan Y Word8 -> Chan Cb Word8 -> Chan Cr Word8 -> Chan G Word8
toG y cb cr = M.zipWith3 (\a b c -> a - b - c) y cb' cr'
  where cb' = M.map (\(fromIntegral -> n) -> round $ 0.34414 * (n - 128)) cb
        cr' = M.map (\(fromIntegral -> n) -> round $ 0.71414 * (n - 128)) cr

toB :: Chan Y Word8 -> Chan Cb Word8 -> Chan B Word8
toB y cb = M.zipWith (+) y cb'
  where cb' = M.map (\(fromIntegral -> n) -> round $ 1.772 * (n - 128)) cb

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
