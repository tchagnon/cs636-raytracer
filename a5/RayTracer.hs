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
import Data.Array
import Debug.Trace

reflectionDepth = 3

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
    writeImage pScene pixels'

-- Split up evaluation of pixels over multiple threads
evalParallel :: Int -> [(Color,Int)] -> [(Color,Int)]
evalParallel threads pixels =
    let chunkSize = (length pixels) `div` threads in
    pixels `using` (parListChunk chunkSize rnf)

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs =  (take n xs):(chunk n (drop n xs))

-- Calculate each ray per pixel and call rayTrace
makePixels :: Scene -> [(Color,Int)]
makePixels scene =
    let (w, h)        = (width scene, height scene) in
    makeRows scene h Nothing

-- Generate pixels, one row at a time
makeRows :: Scene -> Int -> Maybe (Array (Int,Int) Color) -> [(Color,Int)]
makeRows scene 0  lastRowArray = []
makeRows scene k' lastRowArray =
    let (w, h)          = (width scene, height scene) in
    let k               = h-k' in
    let sf              = 2^(superSample scene) in
    let threshold       = sampleThreshold scene in
    let rowArr          = makeRowArray scene k lastRowArray in
    let indices         = [(j,k) | j <- [0..w-1]] in
    (map (avgSamples threshold sf rowArr) indices) ++ (makeRows scene (k'-1) (Just rowArr))

-- Recursive adaptive super-sampling
avgSamples :: Color -> Int -> Array (Int, Int) Color -> (Int, Int) -> (Color,Int)
avgSamples threshold 0  samples (j, k)    = (samples ! (j`div`2 + j`rem`2, k`div`2 + k`rem`2) , 1)
avgSamples threshold sf samples (j, k)    =
    let indices         = [(ji, ki) | ji <- [j,j+1], ki <- [k,k+1]] in
    let nextIndices     = [(ji, ki) | ji <- [2*j,2*j+1], ki <- [2*k,2*k+1]] in
    let localIndices    = [(x*sf, y*sf) | (x,y) <- indices] in
    let [a,b,c,d]       = map (samples!) localIndices in
    let diffPixels      = map abs [a-b, c-d, a-c, b-d] in
    let avgPixels pix   = (1/4) `svMul` (sum pix) in
    if or (map (`colorGreaterThan` threshold) diffPixels)
        then
            let (pix, num) = unzip (map (avgSamples threshold (sf `div` 2) samples) nextIndices) in
            (avgPixels pix, (sum num))
        else
            (avgPixels [a,b,c,d], 4)

-- Construct the super-resolution array of all possible samples for the current row
-- Relies on lazy evaluation to not evaluate makePixel unless needed
-- Uses the bottom samples from the last row if possible
makeRowArray :: Scene -> Int -> Maybe (Array (Int,Int) Color) -> Array (Int, Int) Color
makeRowArray scene row lastRow =
    let (w, h)        = (width scene, height scene) in
    let sf            = 2^(superSample scene) in
    let invSF         = 1/(fromIntegral sf) :: RealT in
    let (wsf, hsf)    = (w*sf, h*sf) in
    let f (j,k)       = makePixel scene ((fromIntegral j)*invSF, (fromIntegral k)*invSF) in
    case lastRow of
        Nothing   -> array ((0,row*sf),(wsf, (row+1)*sf)) 
                     [((j,k),(f (j,k))) | j <- [0..wsf], k <- [row*sf..(row+1)*sf]]
        Just lrow -> array ((0,row*sf),(wsf, (row+1)*sf))
                    ([((j,row*sf),(lrow!(j,row*sf))) | j <- [0..wsf]] ++
                     [((j,k),(f (j,k))) | j <- [0..wsf], k <- [row*sf+1..(row+1)*sf]])
    

-- Construct a single pixel by ray-tracing
makePixel :: Scene -> (RealT, RealT) -> Color
makePixel scene (j, k) =
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
    let ray           = Ray loc (norm (djk j k)) in
    let pixel         = rayTrace scene ray reflectionDepth in
    --trace "1"
    pixel

-- Single Ray Trace
rayTrace :: Scene -> Ray -> Int -> Color
rayTrace scene ray 0        = black
rayTrace scene ray rDepth   =
    let objs        = objects scene in
    let defMat      = defaultMaterial scene in
    let inters      = intersect ray defMat objs in
    getColor scene ray rDepth inters

-- Interpret a list of intersections as a color
getColor :: Scene -> Ray -> Int -> [Intersection] -> Color
getColor scene _         rDepth []       = background scene
getColor scene (Ray o d) rDepth ints     =
    let (Inx t nVec mat)   = minimum ints in
    let ixPt               = o + (t `svMul` d) in
    let ks                 = Material.ks mat in
    let kd                 = Material.kd mat in
    let ka                 = Material.ka mat in
    let cs                 = Material.cs mat in
    let cd                 = Material.cd mat in
    let vVec               = (-d) in
    let rVec               = norm (((2 * (nVec `dot0` vVec)) `svMul` nVec) - vVec) in
    let reflectPoint       = ixPt + (epsilon `svMul` rVec) in
    let iA                 = ambientLight scene in
    let iR                 = rayTrace scene (Ray reflectPoint rVec) (rDepth-1) in
    let diffSpecLights     = map (diffSpec scene mat ixPt nVec vVec) (lights scene) in
    let (specLs, diffLs)   = unzip diffSpecLights in
    let specL              = (ks `svMul` cs) .* (foldl (+) iR specLs) in
    let diffL              = (kd `svMul` cd) .* (foldl (+) iA diffLs) in
    specL + diffL

-- Calculate the specular and diffuse light intesity of a single light
diffSpec :: Scene -> Material -> Vec3f -> Vec3f -> Vec3f -> Light -> (Color, Color)
diffSpec scene mat ixPt nVec vVec light =
    let n                  = Material.n mat in
    let iL                 = color light in
    let lVector            = (position light)-ixPt in
    let lDist              = mag lVector in
    let lVec               = norm lVector in
    let hVec               = norm (lVec + vVec) in
    let cosT               = nVec `dot0` lVec in
    let cosP               = (nVec `dot0` hVec) ** n in
    let reflectPoint       = ixPt + (epsilon `svMul` lVec) in
    let shadowInters       = intersect (Ray reflectPoint lVec) (defaultMaterial scene) (objects scene) in
    let betweenInters      = filter (`intxLessThan` lDist) shadowInters in
    let shadowTransmit     = foldl (*) 1.0 (map (\(Inx _ _ m)-> kt m) betweenInters) in
    if shadowTransmit > 0.0
        then
            ((shadowTransmit * cosP) `svMul` iL, (shadowTransmit * cosT) `svMul` iL)
        else
            (black, black)

-- Take elements from the first list if they exist, otherwise use corresponding elements from 2nd list
elseL :: [a] -> [a] -> [a]
elseL [] ys = ys
elseL (x:xs) (y:ys) = x:(xs `elseL` ys)
