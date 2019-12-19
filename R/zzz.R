.onLoad <- function(libname, pkgname) {
    options("diffdf_options" =  list(
        warnings = TRUE, 
        strict_numeric = TRUE,
        strict_factor = TRUE,
        file = NULL,
        tolerance = sqrt(.Machine$double.eps),
        scale = NULL
    ))


}