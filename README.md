haskell-sdl2gl
==============

Experimenting with Haskell, SDL2 and OpenGL 

SDL2 dependency can be found [here](https://github.com/haskell-game/sdl2) and should be cloned and added as a source using
```
cabal sandbox add-source <sdl2 repo dir>
```

Build
-----

```
$ git clone https://github.com/soupi/haskell-sdl2gl.git
$ git clone https://github.com/haskell-game/sdl2.git
$ cd haskell-sdl2gl
$ cabal sandbox init
$ cabal sandbox add-source ../sdl2
$ cabal install --only-dependencies
$ cabal build
```

Run
---

```
$ cabal run
```
