module Main where

import Data.Matrix as M
import Data.Vector as V
import Data.Word (Word8)
import JPEG
import Test.Tasty
import Test.Tasty.HUnit

---

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Colour Conversions"
    [ testCase "RGB -> YCbCr Isomorphism" $ iso (0,0,0)
    , testCase "RGB -> YCbCr Isomorphism" $ iso (110,115,200)
    , testCase "RGB -> YCbCr Isomorphism" $ iso (255,255,255)
    ]
  ]

iso :: (Word8,Word8,Word8) -> Assertion
iso (x,y,z) = (toR y' cr, toG y' cb cr, toB y' cb) @?= (r,g,b)
  where r = M.fromVector (1,1) $ V.singleton x
        g = M.fromVector (1,1) $ V.singleton y
        b = M.fromVector (1,1) $ V.singleton z
        y' = toY r g b
        cr = toCr r g b
        cb = toCb r g b

main :: IO ()
main = defaultMain suite
