{- Image.hs
 - Write an image to PPM (Portable Pixmap) format.
 - http://en.wikipedia.org/wiki/Portable_pixmap
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Image where

import Color

-- Convert a pixel to PPM P3 format
p3color :: Color -> String
p3color (r, g, b) = unwords [show r, show g, show b]

-- Write pixels to PPM file
writePPM :: String -> Int -> Int -> [Color] -> IO ()
writePPM file width height pixels = do
    let magic = "P3"
    let params = unwords [show x | x <- [width, height, 255]]
    let p3pixels = map p3color pixels
    let outData = unlines ([magic, params] ++ p3pixels)
    writeFile file outData
    return ()

