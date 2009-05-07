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

data ObjectTree = Group [ObjectTree]
                | Transform Mat4f ObjectTree
                | Material Material ObjectTree
                | Primitive Primitive
                | Mesh Mesh
                | LoadMesh String MeshShading
                | Bounding Bounding ObjectTree
    deriving Show

-- Load scene
loadScene :: Scene -> IO Scene
loadScene scene = do
    objs1 <- loadObjs (objects scene)
    return scene{objects = objs1}

-- Load object tree
loadObjs :: ObjectTree -> IO ObjectTree
loadObjs (Group objs)      = do
    objs' <- mapM loadObjs objs
    return (Group objs')
loadObjs (Transform t obj) = do
    obj' <- loadObjs obj
    return (Transform t obj')
loadObjs (Material m obj)  = do
    obj' <- loadObjs obj
    return (Material m obj')
loadObjs (Primitive p)   = return (Primitive p)
loadObjs (LoadMesh file shading) = do
    mesh <- loadSMF file shading
    return (Mesh mesh)
loadObjs (Mesh m)      = error "loadObjs undexpected Mesh"

-- Precompute, flatten and transform; construct Bounding Volume Heirarchy
prepScene :: Scene -> Scene
prepScene scene =
    let objs2 = prepObjs id4f (objects scene) in
    let objs3 = constructBVH objs2 in
    scene{objects = objs3}

-- Push transforms down to primitives
prepObjs :: Mat4f -> ObjectTree -> ObjectTree
prepObjs t (Group objs)        = Group (map (prepObjs t) objs)
prepObjs t (Transform t2 objs) = prepObjs (t `mmMul` t2) objs
prepObjs t (Primitive p)       = Primitive (transformP t p)
prepObjs t (Mesh m)            = Mesh (transformM t m)
prepObjs t (Material m objs)   = Material m (prepObjs t objs)
prepObjs _ o                   = error $ "Undefined prepObjs on object: " ++ (show o)

-- Compute Bounding Volume Heirarchy
constructBVH :: ObjectTree -> ObjectTree
constructBVH (Group objs)        = Group (map constructBVH objs)
constructBVH (Primitive p)       = Bounding (boundingP p) (Primitive p)
constructBVH (Mesh m)            = Bounding (boundingM m) (Mesh m)
constructBVH (Material mat' obj) = Material mat' (constructBVH obj)
constructBVH o                   = error $ "Undefined constructBVH on object: " ++ (show o)

-- Traverse the Object tree looking for intersections
intersect :: Ray -> Material -> ObjectTree -> [Intersection]
intersect r mat (Group objs)        = concatMap (intersect r mat) objs
intersect r mat (Primitive p)       = intersectP r mat p
intersect r mat (Mesh m)            = intersectM r mat m
intersect r mat (Material mat' obj) = intersect r mat' obj
intersect r mat (Bounding b obj)    = if (intersectB r b) then intersect r mat obj else []
intersect _ _   o                   = error $ "Undefined intersect on object: " ++ (show o)

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
