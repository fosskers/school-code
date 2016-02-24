{-# LANGUAGE ViewPatterns #-}

-- |
-- Module    : JPEG
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- An implementation of JPEG encoding and decoding, minus lossless entropy
-- encoding, to demonstrate the main aspects of JPEG compression.

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
         -- * Compression
       , downsample
       , blocks
       , shift
       , dct
       , dct'
       , quantize
         -- * Decompression
       , unquantize
       , idct
       , idct'
       , unshift
       , unblocks
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

-- | The Discrete Cosine Transform. It's math! Woohoo!
-- This is the main source of data loss in the compression process.
-- Input channel `Matrix` must be of size 8x8, or the function will yield
-- `Nothing`. Each `Int` must be 0-centered, for instance by the `shift`
-- function.
dct :: Chan a Int -> Maybe (Chan a Float)
dct c | dim (_mat c) == (8,8) = Just $ dct' c
      | otherwise = Nothing

-- | Oh? Playing with fire? This version won't check the size of the input.
dct' :: Chan a Int -> Chan a Float
dct' (_mat -> c) = Chan $ M.imap f c
  where a 0 = 1 / sqrt 2
        a _ = 1
        f (u,v) _ = 0.25 * a u * a v * (M.foldl (+) 0 $ M.imap g c)
          where g (x,y) p = fi p * cos ((2 * fi x + 1) * fi u * pi / 16)
                                 * cos ((2 * fi y + 1) * fi v * pi / 16)

-- | Center the pixels around 0 before performing the DCT.
shift :: Chan a Int -> Chan a Int
shift = Chan . M.map (\n -> n - 128) . _mat

-- | Quantize a DCT'd channel, given a Quantization Matrix.
quantize :: Matrix Int -> Chan a Float -> Chan a Int
quantize q c = undefined

-- | Reverse a quantization, given the Quantization Matrix that was used
-- during compression.
unquantize :: Matrix Int -> Chan a Int -> Chan a Float
unquantize q c = undefined

-- | The Inverse Discrete Cosine Transform.
-- Input channel `Matrix` must be of size 8x8.
idct :: Chan a Float -> Maybe (Chan a Int)
idct c | dim (_mat c) == (8,8) = Just $ idct' c
       | otherwise = Nothing

-- | I see you like living on the edge. This won't check the input size.
idct' :: Chan a Float -> Chan a Int
idct' = undefined

-- | Restore a shifted channel to its original range of [0-255].
unshift :: Chan a Int -> Chan a Int
unshift = Chan . M.map (\n -> n + 128) . _mat

-- | Revert a List-of-Lists of 8x8 subchannels into their original
-- single Matrix form.
unblocks :: [[Chan a Int]] -> Chan a Int
unblocks cc = Chan $ M.fromBlocks 0 cc'
  where cc' = cc & each . each %~ _mat  -- Should be compiled away.

-- | A shorter alias.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

ex :: Chan a Int
ex = Chan . M.fromVector (8,8) $ V.fromList [ 52,55,61,66,70,61,64,73
                                            , 63,59,55,90,109,85,69,72
                                            , 62,59,68,113,144,104,66,73
                                            , 63,58,71,122,154,106,70,69
                                            , 67,61,68,104,126,88,68,70
                                            , 79,65,60,70,77,68,58,75
                                            , 85,71,64,59,55,61,65,83
                                            , 87,79,69,68,65,76,78,94 ]
