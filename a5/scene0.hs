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
        reflectionDepth = 4,
        background      = black,
        ambientLight    = white,
        defaultMaterial = mat0,
        camera  =
            Camera {
                dist        = 8,
                fovAngle    = 45,
                location    = vec3f 0 3.0 8,
                direction   = vec3f 0 0 (-1),
                up          = vec3f 0 1 0
            },
        lights = [light0],
        objects =
            Group [
                Primitive (Plane (vec3f 0 0 0) (vec3f 0 0 1) (vec3f 1 0 0)),
                Material matMirror (
                    Transform (translate (vec3f 0 2.3 (-1))) trefoil
                ),
                Material matBlue (
                    Transform (translate (vec3f 1.5 0.75 0.5)) (
                    Transform (scale (vec3f 1.5 1.5 1.5)) (
                        LoadMesh "models/bound-bunny_1k.smf" SmoothShade
                    ))
                ),
                Material matTeapot (
                    Transform (translate (vec3f (-1.5) 0 (-2.5))) (
                    Transform (scale (vec3f 0.45 0.45 0.45)) (
                    Transform (rotate (-115) (vec3f 0 1 0)) (
                        LoadMesh "models/teapot.smf" SmoothShade
                    )))
                ),
                Material matCow (
                    Transform (translate (vec3f (1.5) 3.6 (-1.0))) (
                    Transform (scale (vec3f 2 2 2)) (
                    Transform (rotate (-45) (vec3f 0 1 0)) (
                        LoadMesh "models/bound-cow.smf" SmoothShade
                    )))
                )
            ]
    }

ball = Primitive (sphere 0.4)
trefoilPos :: RealT -> Vec3f
trefoilPos t = vec3f ((sin t) + 2*(sin (2*t))) (-((cos t) - 2 * (cos (2*t)))) (sin (3*t))
trefoil = Group [ Transform (translate (trefoilPos t)) ball
                | t <- [0, pi/20  .. 2*pi]
                ]

mat0 =
    PhongMaterial {
        kd = 0.5,
        ks = 0.1,
        ka = 0.1,
        kt = 0.0,
        n  = 30,
        cs = white,
        cd = white
    }

matBlue = mat0 {cd = cornflowerBlue}
matTeapot = mat0 {cd = orange}

matMirror =
    PhongMaterial {
        kd = 0.1,
        ks = 0.9,
        ka = 0.0,
        kt = 0.0,
        n  = 100,
        cs = white,
        cd = white
    }

matCow     =
    PhongMaterial {
        kd = 0.4,
        ks = 0.2,
        ka = 0.4,
        kt = 0.0,
        n  = 5,
        cs  = darkBrown,
        cd  = darkBrown
    }

light0 =
    PointLight {
        color = white,
        position = vec3f 4 4 9
    }
