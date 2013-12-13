Annotations
===========

The telescope problem: how can low-level resource use be revealed in high-level programs?


The Paper
---------

The paper is written in markdown and converted to latex or pdf using `pandoc`.
Please make sure you have `pandoc` and `pandoc-citeproc` installed.

```
cabal install pandoc pandoc-citeproc`
```

See the `Makefile` for details on converting the paper.


Contributing
------------

We need specific versions of `feldspar-langauge` and `feldspar-compiler` for this project.
The versions are tracked using `git submodule`.

Please use `cabal sandbox` when working with the code. Sand-boxing is available from version 1.18 of `cabal-install`.

After checking out the repository, perform the following steps:

```
git submodule update --init --rebase
cabal sandbox init
cabal sandbox add-source feldspar-language
cabal sandbox add-source feldspar-compiler
cabal install --only-dependencies
```
