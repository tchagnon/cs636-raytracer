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
    module Primitive,
    module Material,
    module Mesh,
    module Object
    ) where

import Color
import Ray
import Scene
import Image
import Math
import Primitive
import Material
import Mesh
import Object

import System.Environment
import Control.Parallel.Strategies

-- Top-level Rendering routine
rayTracer :: Scene -> IO()
rayTracer scene = do
    args                <- getArgs
    let args'           = map read args :: [Int]
    let args''          = args' `elseL` [1, 50]
    let threads         = args'' !! 0
    let faceThreshold   = args'' !! 1
    lScene              <- loadScene scene
    let pScene          = prepScene faceThreshold lScene
    let pixels          = makePixels pScene
    let pixels'         = evalParallel threads pixels
    writePPM (outfile pScene) (width pScene) (height pScene) pixels'

-- Split up evaluation of pixels over multiple threads
evalParallel :: Int -> [Color] -> [Color]
evalParallel threads pixels =
    let chunkSize = (length pixels) `div` threads in
    pixels `using` (parListChunk chunkSize rnf)

-- Calculate each ray per pixel and call rayTrace
makePixels :: Scene -> [Color]
makePixels scene =
    let sf            = superSample scene in
    let invSF         = 1/(fromIntegral sf) :: RealT in
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
    let djk j k       = (p00 + (j `svMul` pixelRightVec)
                             - (k `svMul` pixelDownVec))
                        - loc in
    let ray j k       = Ray loc (norm (djk j k)) in
    let subIncr       = [0.0, invSF .. 1.0 - invSF] in
    let subsamples j k= [ray (j+x) (k+y) | x <- subIncr, y <- subIncr] in
    let rays          = [subsamples (fromIntegral j) (fromIntegral k) | k <- [0..h-1], j <- [0..w-1]]  in
    let singlePix rs  = avgPixels (sf^2) (map (rayTrace scene) rs) in
    let pixels        = map singlePix rays in
    pixels 

-- Single Ray Trace
rayTrace :: Scene -> Ray -> Color
rayTrace scene ray =
    let objs        = objects scene in
    let defMat      = defaultMaterial scene in
    let inters      = intersect ray defMat objs in
    getColor ray inters scene

-- Interpret a list of intersections as a color
getColor :: Ray -> [Intersection] -> Scene -> Color
getColor _         [] scene       = background scene
getColor (Ray o d) ints scene     =
    let (Inx t nVec mat)   = minimum ints in
    let ixPt               = o + (t `svMul` d) in
    let ka                 = Material.ka mat in
    let c                  = Material.c mat in
    let iA                 = ambientLight scene in
    let vVec               = (-d) in
    let diffSpecLights     = map (diffSpec mat ixPt nVec vVec) (lights scene) in
    c .* (foldl (+) (ka `svMul` iA) diffSpecLights)

-- Calculate the diffuse and specular light intesity of a single light
diffSpec :: Material -> Vec3f -> Vec3f -> Vec3f -> Light -> Color
diffSpec mat ixPt nVec vVec light =
    let kd                 = Material.kd mat in
    let ks                 = Material.ks mat in
    let n                  = Material.n mat in
    let iL                 = color light in
    let lVec               = norm ((position light)-ixPt) in
    let rVec               = norm (((2 * (nVec `dot0` lVec)) `svMul` nVec) - lVec) in
    let cosT               = nVec `dot0` lVec in
    let cosP               = (rVec `dot0` vVec) ** n in
    (kd * cosT + ks * cosP) `svMul` iL

-- Average pixels after supersampling
avgPixels :: Int -> [Color] -> Color
avgPixels n pixels = (1/(fromIntegral n)) `svMul` (sum pixels)

-- Take elements from the first list if they exist, otherwise use corresponding elements from 2nd list
elseL :: [a] -> [a] -> [a]
elseL [] ys = ys
elseL (x:xs) (y:ys) = x:(xs `elseL` ys)
