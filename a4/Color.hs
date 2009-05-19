{- Color.hs
 - Color/Pixel
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Color where

import Math

type Color = Vec3f

rgbDec :: RealT -> RealT -> RealT -> Color
rgbDec r g b = Vec3f (r/255) (g/255) (b/255)

colorGreaterThan :: Color -> Color -> Bool
(Vec3f a b c) `colorGreaterThan` (Vec3f d e f) =
    a > d || b > e || c > f

threshold = vec3f 0.1 0.1 0.1

-- Constant typical colors
white           :: Color
white           = vec3f 1 1 1
black           :: Color
black           = vec3f 0 0 0
red             :: Color
red             = vec3f 1 0 0
green           :: Color
green           = vec3f 0 1 0
blue            :: Color
blue            = vec3f 0 0 1
orange          :: Color
orange          = vec3f 1 0.5 0
brown           :: Color
brown           = rgbDec 152 118 84
darkBrown       :: Color
darkBrown       = rgbDec 101 67 33
gray            :: Color
gray            = vec3f 0.5 0.5 0.5
brightOrange    :: Color
brightOrange    = vec3f 1 0.8 0
darkGreen       :: Color
darkGreen       = vec3f 0 0.5 0
cornflowerBlue  :: Color
cornflowerBlue  = rgbDec 100 149 237
