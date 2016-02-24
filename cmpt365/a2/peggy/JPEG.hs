{-# LANGUAGE ViewPatterns #-}

-- |
-- Module    : JPEG
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- An implementation of JPEG compression and decompression, minus lossless
-- entropy encoding, to demonstrate the main aspects of JPEG.

module JPEG
       ( -- * Types
         Jpeg(..)
       , jpeg
       , Chan(..)
       , chan
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
         -- * Compression
       , downsample
       , blocks
       , shift
       , dct
       , quantize
         -- * Decompression
       , unquantize
       , idct
       , unshift
       , unblocks
         -- * Quantization Matrices
       , q50
         -- * Type Conversion
       , toJ
       , toImage
       ) where

import           Codec.Picture.Jpg
import           Codec.Picture.Types
import qualified Data.ByteString as B
import           Data.List (groupBy)
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

-- | Create an 8x8 channel `Matrix` from a list of 64 values.
chan :: [a] -> Matrix a
chan = M.fromVector (8,8) . V.fromList

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
  where r' = M.map ((* (0.299 :: Float)) . fi) $ _mat r
        g' = M.map ((* (0.587 :: Float)) . fi) $ _mat g
        b' = M.map ((* (0.114 :: Float)) . fi) $ _mat b

-- | Produce a Blue-Yellow Chrominance Channel from RGB Channels.
-- https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
toCb :: Chan R Int -> Chan G Int -> Chan B Int -> Chan Cb Int
toCb r g b = Chan $ M.zipWith3 (\x y z -> round $ 128 - x - y + z) r' g' b'
  where r' = M.map ((* (0.169 :: Float)) . fi) $ _mat r
        g' = M.map ((* (0.331 :: Float)) . fi) $ _mat g
        b' = M.map ((* (0.500 :: Float)) . fi) $ _mat b

-- | Produce a Red-Green Chrominance Channel from RGB Channels.
-- https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
toCr :: Chan R Int -> Chan G Int -> Chan B Int -> Chan Cr Int
toCr r g b = Chan $ M.zipWith3 (\x y z -> round $ 128 + x - y - z) r' g' b'
  where r' = M.map ((* (0.500 :: Float)) . fi) $ _mat r
        g' = M.map ((* (0.419 :: Float)) . fi) $ _mat g
        b' = M.map ((* (0.081 :: Float)) . fi) $ _mat b

-- | Recover the R Channel.
toR :: Chan Y Int -> Chan Cr Int -> Chan R Int
toR (_mat -> y) (_mat -> cr) = Chan $ M.zipWith (\a b -> a + b) y cr'
  where cr' = M.map (\n -> round $ 1.403 * (fi n - 128)) cr

-- | Recover the G Channel.
toG :: Chan Y Int -> Chan Cb Int -> Chan Cr Int -> Chan G Int
toG (_mat -> y) (_mat -> cb) (_mat -> cr) =
  Chan $ M.zipWith3 (\a b c -> round $ fi a - b - c) y cb' cr'
  where cb' = M.map (\n -> 0.344 * (fi n - 128)) cb
        cr' = M.map (\n -> 0.714 * (fi n - 128)) cr

-- | Recover the B Channel.
toB :: Chan Y Int -> Chan Cb Int -> Chan B Int
toB (_mat -> y) (_mat -> cb) = Chan $ M.zipWith (\a b -> a + b) y cb'
  where cb' = M.map (\n -> round $ 1.773 * (fi n - 128)) cb

-- | Create a `Jpeg`, with its values in the YCbCr colour space from a
-- JuicyPixels RGB `Image`.
toJ :: Image PixelRGB8 -> Jpeg
toJ (Image w h v) = Jpeg w' h' (toY c1 c2 c3) (toCb c1 c2 c3) (toCr c1 c2 c3)
  where w' = n8 w
        h' = n8 h
        n8 n = n - (n `mod` 8)
        m u = M.subMatrix (0,0) (w'-1,h'-1) . M.fromVector (w,h) $ V.map fi u
        (c1,c2,c3) = (V.unzip3 . trips $ VS.convert v) & each %~ Chan . m

-- | Convert our `Jpeg` back to a JuicyPixels RGB `Image`.
toImage :: Jpeg -> Image PixelRGB8
toImage (Jpeg w h y' cb cr) = Image w h v
  where v = VS.fromList $ V.zipWith3 (,,) r g b ^.. traverse . each
        f = V.map fromIntegral . M.flatten . _mat
        r = f $ toR y' cr
        g = f $ toG y' cb cr
        b = f $ toB y' cb

-- | Read an image file into a our `Jpeg` type.
jpeg :: FilePath -> IO (Either String Jpeg)
jpeg fp = do
  f <- B.readFile fp
  pure $ toJ <$> (decodeJpeg f >>= rgb)

-- | Downsample a JPEG to 4:2:0.
downsample :: Jpeg -> Jpeg
downsample = id

-- | Convert a Channel into a List-of-Lists of 8x8 subchannels.
blocks :: Chan a Int -> [[Chan a Int]]
blocks (_mat -> c) = (indexes $ dim c) & each . each %~ Chan . f
  where f (i,j) = M.subMatrix (i,j) (i+7,j+7) c

-- | The starting indices of every 8x8 subchannel within a parent channel's
-- inner `Matrix`.
indexes :: (Int,Int) -> [[(Int,Int)]]
indexes (w,h) = groupBy (\(a,_) (b,_) -> a == b) is
  where is = (,) <$> [0,8..w-8] <*> [0,8..h-8]

-- | Center the pixels around 0 to fix them in the range [-128,127].
-- This should be used before performing the DCT.
shift :: Chan a Int -> Chan a Int
shift = Chan . M.map (\n -> n - 128) . _mat

-- | The Discrete Cosine Transform. It's math! Woohoo!
-- This actually performs an arbitrary-sized DCT and isn't at all
-- contrained to 8x8 dimensions. However, the input `Matrix`
-- must be 0-centered, for instance by the `shift` function.
dct :: Chan a Int -> Chan a Float
dct (_mat -> c) = Chan $ M.imap f c
  where f (u,v) _ = 0.25 * alpha u * alpha v * (M.foldl (+) 0 $ M.imap g c)
          where g (x,y) p = fi p * cos ((2 * fi x + 1) * fi u * pi / 16)
                                 * cos ((2 * fi y + 1) * fi v * pi / 16)

-- | Used in both the DCT and the IDCT.
alpha :: Int -> Float
alpha 0 = 1 / sqrt 2
alpha _ = 1

-- | Quantize a DCT'd channel, given a Quantization Matrix.
-- This is the main source of data loss in the compression process.
quantize :: Matrix Int -> Chan a Float -> Chan a Int
quantize q c = Chan $ M.zipWith (\c' q' -> round $ c' / fi q') (_mat c) q

-- | Reverse a quantization, given the Quantization Matrix that was used
-- during compression.
unquantize :: Matrix Int -> Chan a Int -> Chan a Int
unquantize q c = Chan . M.zipWith (*) q $ _mat c

-- | The Inverse Discrete Cosine Transform.
idct :: Chan a Int -> Chan a Int
idct (_mat -> c) = Chan $ M.imap f c
  where f (x,y) _ = round $ 0.25 * (M.foldl (+) 0 $ M.imap g c)
          where g (u,v) p = alpha u * alpha v * fi p
                            * cos ((2 * fi x + 1) * fi u * pi / 16)
                            * cos ((2 * fi y + 1) * fi v * pi / 16)

-- | Restore a shifted channel to its original range of [0-255].
unshift :: Chan a Int -> Chan a Int
unshift = Chan . M.map (\n -> n + 128) . _mat

-- | Revert a List-of-Lists of 8x8 subchannels into their original
-- single Matrix form.
unblocks :: [[Chan a Int]] -> Chan a Int
unblocks cc = Chan $ M.fromBlocks 0 cc'
  where cc' = cc & each . each %~ _mat  -- Should be compiled away.

-- | A quantization Matrix for 50% quality.
q50 :: Matrix Int
q50 =  chan [ 16,11,10,16,24,40,51,61
            , 12,12,14,19,26,58,60,55
            , 14,13,16,24,40,57,69,56
            , 14,17,22,29,51,87,80,62
            , 18,22,37,56,68,109,103,77
            , 24,35,55,64,81,104,113,92
            , 49,64,78,87,103,121,120,101
            , 72,92,95,98,112,100,103,99 ]

-- | A shorter alias.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
