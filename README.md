#Tree Derivations#

This is an introductory-level example of Haskell programming
using the `diagrams` package to make pictures of fractal-like trees. It
assumes you have the [Haskell Platform][HP] and [git][git], and know how to use a
command line terminal. This file is written in Markdown
syntax so is best viewed directly from GitHub.

To build the programs do the following (the last step may take five minutes or more):

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

The slides (open the file index.html from your local checkout—not directly from GitHub)
describe the relationships between the programs.

The project builds a number of executables, most of which are run with a command line
resembling the following:

```
cabal build
dist/build/stick-figure-0/stick-figure-0 -w 400 -o tree.svg
```

where `stick-figure-0` is the program, `400` is the diagram width, and `foo.svg` is
the output file name.

[HP]: http://www.haskell.org/platform/
[git]: http://git-scm.com
