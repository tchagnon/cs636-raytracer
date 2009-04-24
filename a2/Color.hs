{- Color.hs
 - Color/Pixel
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Color where

import Math

type Color = Vec3f

-- Constant typical colors
white :: Color
white = vec3f 1 1 1
black :: Color
black = vec3f 0 0 0

