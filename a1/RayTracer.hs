{- RayTracer.hs
 - Top-level code.
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module RayTracer (
    rayTracer,
    module Math, 
    module Color,
    module Ray, 
    module Scene,
    module Primitive
    ) where

import Color
import Ray
import Scene
import Image
import Math
import Primitive

import System.Environment
import Control.Parallel.Strategies

--import Debug.Trace
--traceShow :: Show a => a->a
--traceShow x = trace (show x) x

-- Top-level Rendering routine
rayTracer :: Scene -> IO()
rayTracer scene = do
    args                <- getArgs
    let threads         =  read (head (args ++ ["1"])) :: Int
    lScene              <- loadScene scene
    let pScene          = prepScene lScene
    let pixels          = makePixels pScene
    let pixels'         = evalParallel threads pixels
    writePPM (outfile pScene) (width pScene) (height pScene) pixels'

-- Split up evaluation of pixels over multiple threads
evalParallel :: Int -> [Color] -> [Color]
evalParallel threads pixels =
    let chunkSize = (length pixels) `div` threads in
    pixels `using` (parListChunk chunkSize rdeepseq)

-- Calculate each ray per pixel and call rayTrace
makePixels :: Scene -> [Color]
makePixels scene =
    let (w, h)        = (width scene, height scene) in
    let cam           = camera scene in
    let d             = dist cam in
    let theta         = deg2rad (fovAngle cam) in
    let loc           = location cam in
    let dir           = direction cam in
    let upv           = up cam in
    let side          = dir `cross` upv in
    let sj            = 2 * d * (tan (theta/2)) in
    let sk            = sj * ((fromIntegral h) / (fromIntegral w)) in
    let p00           = loc + (d `svMul` dir) - ((sj/2) `svMul` side) + ((sk/2) `svMul` upv) in
    let pixelRightVec = (sj/(fromIntegral (w-1))) `svMul` side in
    let pixelDownVec  = (sk/(fromIntegral (h-1))) `svMul` upv in
    let djk j k       = (p00 + (fromIntegral j) `svMul` pixelRightVec 
                             - (fromIntegral k) `svMul` pixelDownVec)
                        - loc in
    let rays          = [(Ray loc (norm (djk j k))) | k <- [0..h-1], j <- [0..w-1]]  in
    let pixels        = map (rayTrace scene) rays in
    pixels 

-- Single Ray Trace
rayTrace :: Scene -> Ray -> Color
rayTrace scene ray =
    let objs        = objects scene in
    let inters      = intersect ray objs in
    getColor inters scene

-- Interpret a list of intersections as a color
getColor :: [Intersection] -> Scene -> Color
getColor [] scene       = background scene
getColor (i:_) scene    = white
