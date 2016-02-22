{-# LANGUAGE ViewPatterns #-}

module JPEG
       ( -- * Types
         Jpeg(..)
       , jpeg
       , Chan(..)
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
       ) where

import           Codec.Picture.Jpg
import           Codec.Picture.Types
import qualified Data.ByteString as B
import           Data.Matrix as M
import           Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Lens.Micro

---

-- | A Jpeg has a height and width, and three colour channels from the
-- YCbCr colour space.
data Jpeg = Jpeg { _width :: !Int
                 , _height :: !Int
                 , _y' :: Chan Y  Int
                 , _cb :: Chan Cb Int
                 , _cr :: Chan Cr Int } deriving (Eq,Show)

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
-- particular `Num` type. @c@ is a phantom parameter for indicating
-- the specific Channel type.
newtype Chan c a = Chan { _mat :: Matrix a } deriving (Eq,Show)

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
toY :: Chan R Int -> Chan G Int -> Chan B Int -> Chan Y Int
toY r g b = Chan $ M.zipWith3 (\x y z -> round $ x + y + z) r' g' b'
  where r' = M.map ((* (0.299 :: Float)) . fromIntegral) $ _mat r
        g' = M.map ((* (0.587 :: Float)) . fromIntegral) $ _mat g
        b' = M.map ((* (0.114 :: Float)) . fromIntegral) $ _mat b

-- | Produce a Blue-Yellow Chrominance Channel from RGB Channels.
-- https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
toCb :: Chan R Int -> Chan G Int -> Chan B Int -> Chan Cb Int
toCb r g b = Chan $ M.zipWith3 (\x y z -> round $ 128 - x - y + z) r' g' b'
  where r' = M.map ((* (0.169 :: Float)) . fromIntegral) $ _mat r
        g' = M.map ((* (0.331 :: Float)) . fromIntegral) $ _mat g
        b' = M.map ((* (0.500 :: Float)) . fromIntegral) $ _mat b

-- | Produce a Red-Green Chrominance Channel from RGB Channels.
-- https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
toCr :: Chan R Int -> Chan G Int -> Chan B Int -> Chan Cr Int
toCr r g b = Chan $ M.zipWith3 (\x y z -> round $ 128 + x - y - z) r' g' b'
  where r' = M.map ((* (0.500 :: Float)) . fromIntegral) $ _mat r
        g' = M.map ((* (0.419 :: Float)) . fromIntegral) $ _mat g
        b' = M.map ((* (0.081 :: Float)) . fromIntegral) $ _mat b

-- | Recover the R Channel.
toR :: Chan Y Int -> Chan Cr Int -> Chan R Int
toR (_mat -> y) (_mat -> cr) = Chan $ M.zipWith (\a b -> a + b) y cr'
  where cr' = M.map (\(fromIntegral -> n) -> round $ 1.403 * (n - 128)) cr

-- | Recover the G Channel.
toG :: Chan Y Int -> Chan Cb Int -> Chan Cr Int -> Chan G Int
toG (_mat -> y) (_mat -> cb) (_mat -> cr) =
  Chan $ M.zipWith3 (\a b c -> round $ fromIntegral a - b - c) y cb' cr'
  where cb' = M.map (\(fromIntegral -> n) -> 0.344 * (n - 128)) cb
        cr' = M.map (\(fromIntegral -> n) -> 0.714 * (n - 128)) cr

-- | Recover the B Channel.
toB :: Chan Y Int -> Chan Cb Int -> Chan B Int
toB (_mat -> y) (_mat -> cb) = Chan $ M.zipWith (\a b -> a + b) y cb'
  where cb' = M.map (\(fromIntegral -> n) -> round $ 1.773 * (n - 128)) cb

-- | Create a `Jpeg`, with its values in the YCbCr colour space from a
-- JuicyPixels RGB `Image`.
toJ :: Image PixelRGB8 -> Jpeg
toJ (Image w h v) = Jpeg w' h' (toY c1 c2 c3) (toCb c1 c2 c3) (toCr c1 c2 c3)
  where w' = n8 w
        h' = n8 h
        n8 n = n - (n `mod` 8)
        m u = M.subMatrix (0,0) (w'-1,h'-1) . M.fromVector (w,h) $ V.map fromIntegral u
        (c1,c2,c3) = (V.unzip3 . trips $ VS.convert v) & each %~ Chan . m

-- | Read an image file into a our `Jpeg` type.
jpeg :: FilePath -> IO (Either String Jpeg)
jpeg fp = do
  f <- B.readFile fp
  pure $ toJ <$> (decodeJpeg f >>= rgb)

-- | Downsample a JPEG to 4:2:0.
downsample :: Jpeg -> Jpeg
downsample = id
