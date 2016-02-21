import Data.Matrix as M
import JPEG

---

main :: IO ()
main = do
  i <- image "small-kitten.jpeg"
  case i of
    Left err -> putStrLn err
    Right img -> do
      let j = jpeg img
      print $ jWidth j
      print $ jHeight j
      print $ y' j M.! (0,0)
