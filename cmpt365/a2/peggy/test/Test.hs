module Main where

import Data.Matrix as M
import Data.Vector as V
import JPEG
import Test.Tasty
import Test.Tasty.HUnit

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
    [ testCase "8x8" $ toJ' (toImage' j) @?= j
    , testCase "16x16" $ toJ' (toImage' j') @?= j'
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

c :: Chan a Int
c = Chan $ chan [1..64]

c2 :: Chan a Int
c2 = Chan . chan . Prelude.take 64 . Prelude.concat $ repeat [1,2,3]

c3 :: Chan a Int
c3 = Chan . M.fromVector (16,16) $ V.fromList [0..255]

j :: Jpeg
j = Jpeg 8 8 ex ex ex

j' :: Jpeg
j' = Jpeg 16 16 c3 c3 c3

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
