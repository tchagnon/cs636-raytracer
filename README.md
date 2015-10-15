# cs636-raytracer

Raytracer for Drexel CS636 class written in Haskell

This package was written for the Spring 2009 [CS636 Advanced Rendering
Techniques][class] class at Drexel University, taught by David Breen.  I am
posting it here in its original form with original git history.  Each assignment
code is in a separate directory and builds on the previous assignments.  The
class web page has all the assignment requirement details, lectures and
background information.

Each assignment directory (a1-a6) has its own simple Makefile build and README.

## Building

All executables can now be built with [stack][stack].  After cloning this repo,
run:
```
stack setup
stack build
```

[stack]:https://github.com/commercialhaskell/stack

## Running

To run each scene executable, time it and display the resulting image, execute
the following.  The `4 +RTS -N4` specifies the number of threads to use.
```
time stack exec a4-scene0 4 +RTS -N4
convert scene0.ppm scene0.png
open scene0.png
```

## Links

- [Class website, lectures and assignments][class]
- [My class project page][project], containing:
 - Rendered images
 - Performance details
 - Notes on code changes

[class]: https://www.cs.drexel.edu/~david/Classes/CS431/index_Spring09.html
[project]: https://www.cs.drexel.edu/~tc365/cs636.html
