## Release summary
In this version I have:
- Updated the package to use suppressWarnings(RNGversion("3.5.0")) wherever set.seed() has been used to make sure test results and vignette results are consistent after the planned changes to the random number generator 

## Test environments
- local Mac OS Mojave R 3.5.2
- Win-Builder R-release
- Debian Linux R-release (via rhub)

## R CMD check results
R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
There are currently no downstream dependencies for this package