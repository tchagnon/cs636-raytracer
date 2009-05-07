{- Scene.hs
 - Heirarchical Scene Description
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}
module Scene where

import Color
import Math
import Ray
import Primitive
import Mesh
import Material
import Bounding
import Object

data Scene =
    Scene {
        outfile         :: String,
        width           :: Int,
        height          :: Int,
        superSample     :: Int,
        background      :: Color,
        ambientLight    :: Color,
        defaultMaterial :: Material,
        camera          :: Camera,
        lights          :: [Light],
        objects         :: ObjectTree
    }
    deriving Show

data Camera = 
    Camera {
        dist        :: RealT,
        fovAngle    :: RealT,
        location    :: Vec3f,
        direction   :: Vec3f,
        up          :: Vec3f
    }
    deriving Show

data Light =
    PointLight{
        color       :: Color,
        position    :: Vec3f
    }
    deriving Show

-- Load scene
loadScene :: Scene -> IO Scene
loadScene scene = do
    objs1 <- loadObjs (objects scene)
    return scene{objects = objs1}

-- Precompute, flatten and transform; construct Bounding Volume Heirarchy
prepScene :: Scene -> Scene
prepScene scene =
    let objs2 = prepObjs id4f (objects scene) in
    let objs3 = constructBVH objs2 in
    scene{objects = objs3}

-- Generate a translation transformation matrix
translate :: Vec3f -> Mat4f
translate v =
    let (x, y, z) = vec3fElts v in
    mat4f   (vec4f 1 0 0 x)
            (vec4f 0 1 0 y)
            (vec4f 0 0 1 z)
            (vec4f 0 0 0 1)

-- Generate a scaling transformation matrix
scale :: Vec3f -> Mat4f
scale v =
    let (x, y, z) = vec3fElts v in
    mat4f   (vec4f x 0 0 0)
            (vec4f 0 y 0 0)
            (vec4f 0 0 z 0)
            (vec4f 0 0 0 1)

-- Generate a rotation transformation matrix
-- First arg is angle in degrees
-- Second arg is a vector around which to rotate
rotate :: RealT -> Vec3f -> Mat4f
rotate theta v =
    let (x, y, z) = vec3fElts (norm v) in
    let c = cos (deg2rad theta) in
    let s = sin (deg2rad theta) in
    mat4f   (vec4f (x^2+(1-x^2)*c)  (x*y*(1-c)-z*s) (x*z*(1-c)+y*s) 0)
            (vec4f (x*y*(1-c)+z*s)  (y^2+(1-y^2)*c) (y*z*(1-c)-x*s) 0)
            (vec4f (x*z*(1-c)-y*s)  (y*z*(1-c)+x*s) (z^2+(1-z^2)*c) 0)
            (vec4f 0                0               0               1)
