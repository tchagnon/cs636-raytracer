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
cornflowerBlue  :: Color
cornflowerBlue  = rgbDec 100 149 237
