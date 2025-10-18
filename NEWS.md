

# diffdf 1.1.2

Mostly just bug fixes:
- Fixed bug of where an internal function was reliant on a partial argument match (#138)
- Fixed bug where `diffdf()` would error if the input was a complex expression (#133)
- Fixed bug where `diffdf()` would error if an input dataset contained a POSIXct column which contains a missing value (#132)
- Added argument to `print()` function to allow users to print more than 10 rows at once when creating a file output (#135)


# diffdf 1.1.1

Fix minor bug with CRAN submission (missing file from `.Rbuildignore`)


# diffdf 1.1.0

## New features

- Enhanced table printing so that white space characters are more clearly displayed (#87)
- Added `row_limit` argument to print method to limit the number of rows displayed (#6, @brianrepko)
- Added check to ensure that the column ordering is the same (#32)
- Added check for differences in classes between the base and comparison datasets (#42)
- Updated character formatting of datetimes to show time zone to avoid misleading/confusing comparisons (#121)

## Minor improvements and fixes

- Added more informative error messaging if a specified key is missing from the base or comparison dataset (#113)
- Fixed bug that caused an error if either the base or comparison dataset were empty (#44)
- Fixed bug that caused an error if there were no matching keys between the base and comparison dataset (#79)


# diffdf 1.0.4

- No new functionality
- Update to make package compatible with tibble 3.0.0

# diffdf 1.0.3

- No new functionality
- Update to make package compatible with the changes made to sampling within core R

# diffdf 1.0.2

- No new functionality
- Update to make package compatible with tibble 2.0.0

# diffdf 1.0.1

- Added package down site  
- Updated description file to include unnamed dependencies in test files
- Added Travis 
- Added CodeCov

# diffdf 1.0.0 

- Initial Release !!
