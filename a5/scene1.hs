{- scene1.hs
 - ...
 -
 - Timothy A. Chagnon
 - CS 636 - Spring 2009
 -}

import RayTracer

main = rayTracer scene

scene =
    Scene {
        outfile         = "scene1.ppm",
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
                location    = vec3f 0 3 5,
                direction   = norm (vec3f 0 (-0.5) (-1)),
                up          = norm (vec3f 0 1 (-0.5))
            },
        lights = [light0, lightFire],
        objects =
            Group [
                -- Coordinate grid for testing
                -- Group [
                --    Transform (translate (vec3f x 0 z)) (Primitive (sphere 0.05))
                --    | x <- [-5..5], z <- [-20..0]
                --],
                Material matRocks (
                    Group [
                        Transform (translate (vec3f (cos t) 0 (sin t))) (Primitive (sphere 0.08))
                        | t <- [0, pi/6 .. 2*pi]
                    ]
                ),
                Material matBunny (
                    Transform (translate (vec3f 1.5 0.5 1.5)) (
                    Transform (rotate (-45) (vec3f 0 1 0)) (
                        LoadMesh "../models/bound-bunny_1k.smf" FlatShade
                    ))
                ),
                Material matFire (
                    Transform (translate (vec3f 0.3 0 0)) (
                    Transform (scale (vec3f 0.007 0.007 0.007)) (
                    Transform (rotate (-90) (vec3f 1 0 0)) (
                        LoadMesh "../models/campfire.smf" SmoothShade
                    )))
                ),
                Material matFrog (
                    Transform (translate (vec3f (1.7) 0.5 (-1.7))) (
                    Transform (scale (vec3f 0.5 0.5 0.5)) (
                    Transform (rotate 45 (vec3f 0 1 0)) (
                        LoadMesh "../models/frog.smf" SmoothShade
                    )))
                ),
                Material matOuthouse (
                    Transform (translate (vec3f 3 0 (-8))) (
                    Transform (scale (vec3f 0.02 0.02 0.02)) (
                    Transform (rotate (-35) (vec3f 0 1 0)) (
                    Transform (rotate (-90) (vec3f 1 0 0)) (
                        LoadMesh "../models/outhouse.smf" SmoothShade
                    ))))
                ),
                Material matCow (
                    Transform (translate (vec3f (-1.5) 0.6 (-1.5))) (
                    Transform (scale (vec3f 2 2 2)) (
                    Transform (rotate (-45) (vec3f 0 1 0)) (
                        LoadMesh "../models/bound-cow.smf" SmoothShade
                    )))
                ),
                Material mat0 (
                    Transform (translate (vec3f (-1.3) 0 (1.3))) (
                    Transform (scale (vec3f 0.15 0.15 0.15)) (
                    Transform (rotate (45) (vec3f 0 1 0)) (
                        LoadMesh "../models/teapot.smf" SmoothShade
                    )))
                )

            ]
    }

lightFire =
    PointLight {
        color = 2 `svMul` orange,
        position = vec3f 0 1 0
    }

light0 =
    PointLight {
        color = 0.5 `svMul` white,
        position = vec3f 4 4 9
    }

mat0 =
    PhongMaterial {
        kd = 0.7,
        ks = 0.3,
        ka = 0.1,
        kt = 0.0,
        n  = 30,
        cs = white,
        cd = white
    }

matRocks = mat0 {cd = gray}
matBunny = mat0 {cd = cornflowerBlue}

matFire     =
    PhongMaterial {
        kd = 0.8,
        ks = 0.1,
        ka = 0.5,
        kt = 1.0,
        n  = 5,
        cs = white,
        cd = orange
    }

matFrog =
    PhongMaterial {
        kd = 0.6,
        ks = 1.0,
        ka = 0.3,
        kt = 0.0,
        n  = 70,
        cs = white,
        cd = darkGreen
    }

matOuthouse     =
    PhongMaterial {
        kd = 0.4,
        ks = 0.2,
        ka = 0.4,
        kt = 0.0,
        n  = 5,
        cs = white,
        cd = brown
    }

matCow = matOuthouse {cd = darkBrown}
