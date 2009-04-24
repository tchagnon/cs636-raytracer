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
        background = black,
        ambientLight    = vec3f 0.2 0.2 0.2,
        defaultMaterial = mat0,
        camera  =
            Camera {
                dist        = 3,
                fovAngle    = 56,
                location    = vec3f 0 1.5 8,
                direction   = vec3f 0 0 (-1),
                up          = vec3f 0 1 0
            },
        lights = [],
        objects =
            Group [
                LoadMesh "../models/teapot.smf"
            ]
    }

mat0 =
    PhongMaterial {
        kd = 0.7,
        ks = 0.3,
        ka = 0.2,
        n  = 30,
        c  = white
    }
