#' diffdf_has_issues
#' 
#' Utility function which returns TRUE if an diffdf
#' object has failed any its checks or TRUE if an diffdf object has passed all of its checks
#' @param x diffdf object
#' @examples
#' 
#' # Example with no issues
#' x <- diffdf( iris, iris )
#' diffdf_has_issues(x)
#' 
#' # Example with issues
#' iris2 <- iris
#' iris2[2,2] <- NA
#' x <- diffdf( iris , iris2 , onfailure = "nothing")
#' diffdf_has_issues(x)
#' @export
diffdf_has_issues <- function(x){
    return( x$result != "Passed")
}


#' diffdf_has_passed
#' 
#' Utility function which returns TRUE if an diffdf
#' object has passed all its checks or FALSE if an diffdf object has failed any of its checks
#' @param x diffdf object
#' @examples
#' 
#' # Example with no issues
#' x <- diffdf( iris, iris )
#' diffdf_has_passed(x)
#' 
#' # Example with issues
#' iris2 <- iris
#' iris2[2,2] <- NA
#' x <- diffdf( iris , iris2 , onfailure = "nothing")
#' diffdf_has_passed(x)
#' @export
diffdf_has_passed <- function(x){
    return( x$result == "Passed")
}
