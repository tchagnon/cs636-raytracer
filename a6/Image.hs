{- Image.hs
 - Write an image to PPM (Portable Pixmap) format.
 - http://en.wikipedia.org/wiki/Portable_pixmap
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Image where

import Color
import Math
import Scene

-- Convert a pixel to PPM P3 format
p3color :: Color -> String
p3color (Vec3f r g b) = unwords [showC r, showC g, showC b] where
    showC c = show $ floor $ 255 * (min 1 c)

writeImage :: Scene -> [(Color,Int)] -> IO ()
writeImage scene pixels = do
    let (w, h)      = (width scene, height scene)
    let file        = outfile scene
    let sf          = 2^(superSample scene)
    let maxC        = 4*((fromIntegral sf)^2) :: RealT
    let (pix, cnts) = unzip pixels
    if outputSampleMap scene
        then do
            putStrLn $ "Total samples: " ++ (show (sum cnts))
            writePPM file w h (map (cnt2pix maxC) cnts)
        else
            writePPM file w h pix

-- Write pixels to PPM file
writePPM :: String -> Int -> Int -> [Color] -> IO ()
writePPM file width height pixels = do
    let magic    = "P3"
    let params   = unwords [show x | x <- [width, height, 255]]
    let p3pixels = map p3color pixels
    let outData  = unlines ([magic, params] ++ p3pixels)
    writeFile file outData
    return ()

cnt2pix :: RealT -> Int -> Color
cnt2pix sf cnt =
    let c       = (fromIntegral cnt)/sf in
    Vec3f c c c
