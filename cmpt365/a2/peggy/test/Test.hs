module Main where

import           Codec.Picture.Types
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           JPEG
import           Lens.Micro
import           Test.Tasty
import           Test.Tasty.HUnit

---

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Colour Conversions"
    [ testCase "RGB -> YCbCr Isomorphism" $ iso (0,0,0)
    , testCase "RGB -> YCbCr Isomorphism" $ iso (110,115,200)
    , testCase "RGB -> YCbCr Isomorphism" $ iso (50,100,50)
    , testCase "RGB -> YCbCr Isomorphism" $ iso (1,1,1)
    , testCase "RGB -> YCbCr Isomorphism" $ iso (255,0,255)
    , testCase "RGB -> YCbCr Isomorphism" $ iso (123,222,84)
    , testCase "RGB -> YCbCr Isomorphism" $ iso (255,255,255)
    ]
  , testCase "Block Splitting" $ (unblocks $ blocks c) @?= c
  , testCase "Block Splitting" $ (unblocks $ blocks c3) @?= c3
  , testCase "Shifting" $ (unshift $ shift c) @?= c
  , testGroup "Image Conversion"
    [ testGroup "toJ' . toImage'"
      [ testCase "8x8" $ toJ' (toImage' j) @?= j
      , testCase "16x16" $ toJ' (toImage' j') @?= j'
      , testCase "32x32" $ toJ' (toImage' j'') @?= j''
      , testCase "40x16" $ toJ' (toImage' j''') @?= j'''
      , testCase "160x288" $ toJ' (toImage' j'''') @?= j''''
      ]
    , testGroup "toImage' . toJ'"
      [ testCase "8x8" $ imageData (toImage' $ toJ' i01) @?= imageData i01
--      , testCase "10x10" $ imageData (toImage' $ toJ' i02) @?= imageData i02
      , testCase "10x10 (by length)" ia
      , testCase "16x16" $ imageData (toImage' $ toJ' i03) @?= imageData i03
      , testCase "32x8" $ imageData (toImage' $ toJ' i04) @?= imageData i04
      ]
    ]

  ]

iso :: (Int,Int,Int) -> Assertion
iso (x,y,z) = (toR y' cr, toG y' cb cr, toB y' cb) @?= (r,g,b)
  where r = Chan $ M.fromVector (1,1) $ V.singleton x
        g = Chan $ M.fromVector (1,1) $ V.singleton y
        b = Chan $ M.fromVector (1,1) $ V.singleton z
        y' = toY r g b
        cb = toCb r g b
        cr = toCr r g b

cx :: (Int,Int) -> [Int] -> Chan a Int
cx (w,h) l = Chan . M.fromVector (w,h) . V.fromList . take (w*h) . concat $ repeat l

c :: Chan a Int
c = Chan $ chan [1..64]

c2 :: Chan a Int
c2 = cx (8,8) [1,2,3]

c3 :: Chan a Int
c3 = Chan . M.fromVector (16,16) $ V.fromList [0..255]

j :: Jpeg
j = Jpeg 8 8 ex ex ex

j' :: Jpeg
j' = Jpeg 16 16 c3 c3 c3

j'' :: Jpeg
j'' = Jpeg 32 32 (cx (32,32) [1,2,3]) (cx (32,32) [2,3,4]) (cx (32,32) [7,8,9])

j''' :: Jpeg
j''' = Jpeg 40 16 (cx (40,16) [1,2,3]) (cx (40,16) [6,5,4,3,2]) (cx (40,16) [1,1,1])

j'''' :: Jpeg
j'''' = Jpeg 160 288 (cx (160,288) [1,5,4]) (cx (160,288) [10,100,200]) (cx (160,288) [77,77,77])

i01 :: Image PixelYCbCr8
i01 = Image 8 8 $ VS.fromList l
  where l = (zip3 l' l' l') ^.. traverse . each
        l' = [50..113]

i02 :: Image PixelYCbCr8
i02 = Image 10 10 $ VS.fromList l
  where l = (zip3 l' l' l') ^.. traverse . each
        l' = [50..149]

i03 :: Image PixelYCbCr8
i03 = Image 16 16 $ VS.fromList l
  where l = (zip3 l' l' l') ^.. traverse . each
        l' = [0..255]        

i04 :: Image PixelYCbCr8
i04 = Image 32 8 $ VS.fromList l
  where l = (zip3 l' l' l') ^.. traverse . each
        l' = [0..255]

ia :: Assertion
ia = VS.length (imageData . toImage' $ toJ' i02) @?= 64 * 3

ex :: Chan a Int
ex = Chan $ chan [ 52,55,61,66,70,61,64,73
                 , 63,59,55,90,109,85,69,72
                 , 62,59,68,113,144,104,66,73
                 , 63,58,71,122,154,106,70,69
                 , 67,61,68,104,126,88,68,70
                 , 79,65,60,70,77,68,58,75
                 , 85,71,64,59,55,61,65,83
                 , 87,79,69,68,65,76,78,94 ]

main :: IO ()
main = defaultMain suite
