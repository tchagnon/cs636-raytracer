SCENES = scene0
IMAGES = $(addsuffix .ppm,$(SCENES))
DEPENDS = Bounding Color Image Material Math Mesh Object Primitive Ray RayTracer Scene
GHCARGS = $(THREADING) $(PROFILING) $(OPTIMIZE)
THREADING = -threaded
#PROFILING = -prof -auto-all
OPTIMIZE = -O2 -funbox-strict-fields -fexcess-precision -funfolding-keeness-factor=10 $(VIAC)
VIAC = -fvia-C -optc-O3 -optc-msse3 -optc-ffast-math
THREADS = 2

.PHONY: all test clean

all: $(SCENES)

%: %.hs  $(addsuffix .hs,$(DEPENDS))
	ghc $(GHCARGS) --make $@

%.ppm: %
	time -p ./$< $(THREADS) +RTS -N$(THREADS)
	display $@

test: $(IMAGES)

clean:
	-rm -f *.hi *.o *.ppm $(SCENES)
