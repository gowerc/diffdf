
#' diffopts
#'
#' Description - TODO 
#' @param scale TODO 
#' @param tolerance TODO 
#' @param onfailure TODO
#'  
#' @export
diffopts <- function(
    scale = 1,
    tolerance = sqrt(.Machine$double.eps),
    onfailure = "warning"
){
    list(
        scale = scale,
        tolerance = tolerance,
        onfailure = onfailure
    )
}











