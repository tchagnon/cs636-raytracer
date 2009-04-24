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
cornflowerBlue  :: Color
cornflowerBlue  = rgbDec 100 149 237
