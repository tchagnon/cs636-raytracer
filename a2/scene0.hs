{- scene0.hs
 - Scene given on Assignment 1 webpage, 2 spheres and bunny
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}

import RayTracer

main = do
    rayTracer scene0

scene0 =
    Scene {
        outfile         = "scene0.ppm",
        width           = 512,
        height          = 512,
        background      = black,
        ambientLight    = vec3f 0.2 0.2 0.2,
        defaultMaterial = mat0,
        camera  =
            Camera {
                dist        = 3,
                fovAngle    = 56,
                location    = vec3f 0 0 3,
                direction   = vec3f 0 0 (-1),
                up          = vec3f 0 1 0
            },
        lights = [],
        objects =
            Group [
                Transform (translate (vec3f (-1) 0 0)) (
                    Primitive (sphere 0.4)),
                Transform (translate (vec3f   1  0 0)) (
                    Primitive (sphere 0.4)),
                Material matBlue (
                    Transform (rotate (-90) (vec3f  0 1 0)) (
                        --LoadMesh "../models/octahedron.smf"
                        --LoadMesh "../models/icos.smf"
                        --LoadMesh "../models/sprellpsd.smf"
                        --LoadMesh "../models/bound-bunny_200.smf"
                        LoadMesh "../models/bound-bunny_1k.smf"
                        )
                )
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

matBlue = mat0 {c = cornflowerBlue}
