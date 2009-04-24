{- Color.hs
 - Color/Pixel
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Color where

type Color = (Int, Int, Int)

-- Constant typical colors
white :: Color
white = (255, 255, 255)
black :: Color
black = (0, 0, 0)

