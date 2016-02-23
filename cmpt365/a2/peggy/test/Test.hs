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
  , testGroup "Block Splitting"
    [ testCase "Isomorphism" bSplit
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

bSplit :: Assertion
bSplit = (unblocks $ blocks c) @?= c

c :: Chan a Int
c = Chan . M.fromVector (8,8) $ V.fromList [1..64]

main :: IO ()
main = defaultMain suite
