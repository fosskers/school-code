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
    [ testCase "RGB -> YCbCr Isomorphism" iso
    ]
  ]

--iso :: Bool
iso :: Assertion
iso = (r,g,b) @?= (toR y' cr, toG y' cb cr, toB y' cb)
  where r = M.fromVector (1,1) $ V.singleton 110
        g = M.fromVector (1,1) $ V.singleton 115
        b = M.fromVector (1,1) $ V.singleton 200
        y' = toY r g b
        cr = toCr r g b
        cb = toCb r g b

main :: IO ()
main = defaultMain suite
