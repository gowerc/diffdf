

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











