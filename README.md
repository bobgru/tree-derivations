#Tree Derivations#

##Introduction##

This is an introductory/intermediate-level example of Haskell programming
using the `diagrams` package to make pictures of fractal-like trees. It
assumes you have the [Haskell Platform][HP] and [git][git], and know how to use a
command line terminal. This file and the tutorial itself are written in Markdown
syntax so are best viewed in a browser alongside an open terminal window.

To get ready for the tutorial do the following (the last step may take five minutes or more):

```
git clone http://github.com/bobgru/tree-derivations.git
cd tree-derivations
cabal sandbox init
cabal install --dependencies-only
```

If `cabal` complains that you need to update your package database, or that
there is a newer version of `cabal-installer` available, you can run the following
to satisfy it:

```
cabal update
cabal install cabal-installer
```

You now have all the project files and build dependencies.

The tutorial is organized into steps as follows:

0. Stick figure
1. Refactoring—Preparing for Tapered
2. Tapered trunks
3. Refactoring—Preparing for 3D
4. Planar 3D trees
5. Deep 3D trees
6. Flexible input (TBD)
7. Random variations (TBD)

The project builds a number of executables, most of which are run with a command line
resembling the following:

```
cabal build
dist/build/stick-figure-0/stick-figure-0 -w 400 -o tree.svg
```

where `stick-figure-0` is the program, `400` is the diagram width, and `foo.svg` is
the output file name.

A slide presentation describing the programs is available [here][slides].

[HP]: http://www.haskell.org/platform/
[git]: http://git-scm.com
[slides]: http://github.com/bobgru/tree-derivations/index.html
