{- scene0.hs
 - Scene given on Assignment 1 webpage, 2 spheres and bunny
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}

import RayTracer

main = rayTracer scene0

scene0 =
    Scene {
        outfile         = "scene0.ppm",
        width           = 512,
        height          = 512,
        superSample     = 2,
        outputSampleMap = False,
        sampleThreshold = vec3f 0.05 0.05 0.05,
        background      = black,
        ambientLight    = white,
        defaultMaterial = mat0,
        camera  =
            Camera {
                dist        = 5,
                fovAngle    = 45,
                location    = vec3f 0 0 5,
                direction   = vec3f 0 0 (-1),
                up          = vec3f 0 1 0
            },
        lights = [light0],
        objects =
            Group [
                Transform (translate (vec3f (-1) 1 0)) (
                    Primitive (sphere 0.4)),
                Transform (translate (vec3f   1  1 0)) (
                    Primitive (sphere 0.4)),
                Material matBlue (
                    Transform (translate (vec3f 0 (-0.3) 2.5)) (
                        --LoadMesh "../models/octahedron.smf" FlatShade
                        --LoadMesh "../models/icos.smf" FlatShade
                        --LoadMesh "../models/sprellpsd.smf" FlatShade
                        --LoadMesh "../models/bound-bunny_200.smf" SmoothShade
                        LoadMesh "../models/bound-bunny_1k.smf" SmoothShade
                    )
                )
            ]
    }

mat0 =
    PhongMaterial {
        kd = 0.7,
        ks = 0.3,
        ka = 0.1,
        kt = 0.0,
        n  = 30,
        c  = white
    }

matBlue = mat0 {c = cornflowerBlue}

light0 =
    PointLight {
        color = white,
        position = vec3f 4 4 9
    }
