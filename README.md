annotations
===========

The telescope problem: how can low-level resource use be revealed in high-level programs?


Contributing
------------

We need specific versions of `feldspar-langauge` and `feldspar-compiler` for this project.
The versions are tracked using `git submodule`.

Plese use `cabal sandbox` when working with the code. Sandboxing is available from version 1.18 of `cabal-install`.

After checking out the repo, perform the following steps:

```
git submodule update --init --rebase
cabal sandbox init
cabal sandbox add-source feldspar-language
cabal sandbox add-source feldspar-compiler
cabal install --only-dependencies
```
