{- teapot.hs
 - Scene containing the teapot.
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}

import RayTracer

main = do
    rayTracer scene

scene =
    Scene {
        outfile = "teapot.ppm",
        width   = 512,
        height  = 512,
        camera  =
            Camera {
                dist        = 3,
                fovAngle    = 56,
                location    = vec3f 0 1.5 8,
                direction   = vec3f 0 0 (-1),
                up          = vec3f 0 1 0
            },
        background = black,
        objects =
            Group [
                LoadMesh "models/teapot.smf"
            ]
    }
