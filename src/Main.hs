{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}

module Main where

import           Control.Arrow
import           Data.Coerce
import           Data.Function
import           Data.List
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           Formatting                     hiding (text)
import           Linear.Matrix

add1 :: Num a => V2 a -> V3 a
add1 (V2 x y) = V3 x y 1

perpDist :: (Num a, Fractional a, Floating a) => V2 a -> V2 a -> V2 a -> a
perpDist (add1 -> begin) (add1 -> end) (add1 -> point) =
    abs $ det33 (V3 begin end point) / distanceA begin end

rdp :: (Ord a, Floating a) => a -> [V2 a] -> [V2 a]
rdp epsilon line
  | length line > 2 =
    let middle = tail $ init line
        (begin, end) = (head &&& last) line
        measured = fmap (perpDist begin end) middle
        (dist, os) = maximumBy (compare `on` fst) (zip measured [2..])
    in if dist > epsilon
       then let (l,r) = splitAt os line
            in init (rdp epsilon l) <> rdp epsilon r
       else [begin, end]
  | otherwise = line

input :: [V2 Double]
input =
    let dampedOscillator t = exp (negate t) * cos (2*pi*t)
    in [V2 x (dampedOscillator x) | x <- [0, 0.01 .. 5] ]

renderate :: Double -> [V2 Double] -> QDiagram Cairo V2 Double Any
renderate epsilon xs =
    topLeftText ("\tf(x)\t= e⁻ˣcos(2πx), x ∈ [0, 5]\n" <> status)
    === coerce xs # fromVertices # strokeLine
  where
    status = formatToString ("\tε \t= " % fixed 3 % "\t\t" % "n = " % int)
                            epsilon (length xs)

main :: IO ()
main =
  gifMain [ (renderate epsilon (rdp epsilon input) # bgFrame 0.1 white
                                                   # fontSizeG 0.2
                                                   # font "dejaVuSans"
            , 10 )
           | epsilon <- [0, 0.001 .. 0.075] ]
