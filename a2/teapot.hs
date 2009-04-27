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
        outfile         = "teapot.ppm",
        width           = 512,
        height          = 512,
        superSample     = 2,
        background      = black,
        ambientLight    = white,
        defaultMaterial = mat0,
        camera  =
            Camera {
                dist        = 3,
                fovAngle    = 47,
                location    = vec3f 0 1.5 8,
                direction   = vec3f 0 0 (-1),
                up          = vec3f 0 1 0
            },
        lights = [light0, light1],
        objects =
            Group [
                LoadMesh "../models/teapot.smf" SmoothShade
            ]
    }

mat0 =
    PhongMaterial {
        kd = 0.7,
        ks = 0.3,
        ka = 0.1,
        n  = 30,
        c  = white
    }

light0 =
    PointLight {
        color = white,
        position = vec3f 3 3 3
    }

light1 =
    PointLight {
        color = red,
        position = vec3f (-4) 0 9
    }
