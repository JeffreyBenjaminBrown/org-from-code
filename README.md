# Purpose

Run from the root of a code repo,
this will find all .rs (and soon, all .el) files,
and build an org document corresponding to the filestructure,
with each file somewhere in that document,
and each function definition from a file
contained as a sub-heading below that file's heading.

# Usage

Build the executable via
```
cabal install --install-method=copy --installdir=.
```
or, to overwrite the prior executable,
```
cabal install --install-method=copy --installdir=. \
  --overwrite-policy=always
```

Run that executable from the root of the code repo of interest. The result will be (over)written to a file called `./all-code.org`.
