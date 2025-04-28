## Test environments
* win-builder (devel and release)
* local OS X install, R 4.4.3
* local Linux, R 4.4.3

## R CMD check results
0 errors | 0 warnings | 1 notes

The note is related to the installed package size. This is primarily due to the inclusion of the `bin` sub-directory, which contains precompiled `CmdStan` models obtained using the `instantiate` package.
This functionality is the key innovation in this version.

## Downstream dependencies
There are currently no downstream dependencies for this package.


Thank you

