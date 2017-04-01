Some experiments with fractals
for [a Haskell course on IMD-UFRN](http://haskell.imd.ufrn.br).

In src/Fractal.hs we build a fractal (which is an infinite tree), then
turn it into an infinite list of line segments.

In app/Main.hs this list is consumed in an animation that builds the
fractal piece by piece.

How to compile and run: install stack, then ./run
