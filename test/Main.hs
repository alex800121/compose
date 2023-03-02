module Main (main) where

import Test.QuickCheck
import Compose
import qualified Data.Composition as C

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

main :: IO ()
main = quickCheck $ verbose $
  (\x -> f x === (show <$> safeHead x))
  .&&. (\y z -> g y z === (show C..:: zipWith3) foldr a y z)
  .&&. (\x y -> map (+ x) y === h x y)
  where
    f :: String -> Maybe String
    f = fmap show .: safeHead
    g :: [Int] -> [[Int]] -> String
    g = (show .: zipWith3) foldr a
    a = [(+), (*), (-), const]
    h :: Int -> [Int] -> [Int]
    h = (map .: (+))
