
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
    onfailure = "warning",
    ...
){
    
    dots <- list(...)
    if( length(dots) != 0){
        stop("The following options are invalid: ", paste0(names(dots), collapse = " "), call. = FALSE)
    }
    
    if( !( is.numeric(scale) & length(scale) == 1)){
        stop("'scale' should be numeric", call. = FALSE)
    }
    
    if( !( is.numeric(tolerance) & length(tolerance) == 1)){
        stop("'tolerance' should be numeric", call. = FALSE)
    }
    
    if( !( onfailure %in% c("warning", "message", "error", "nothing") & length(onfailure) == 1)){
        stop("'onfailure' should be one of ['warning', 'message', 'error', 'nothing']", call. = FALSE)
    }
    
    list(
        scale = scale,
        tolerance = tolerance,
        onfailure = onfailure
    )
}

#' merge_options
#' 
#' Description - TODO
#' @param opts TODO
#' @param ... TODO
merge_options <- function(opts = NULL, ...){
    dots <- list(...)
    if( is.null(opts)) opts <- list()
    for( i in names(dots)) opts[[i]] <- dots[[i]]
    opts2 <- do.call(diffopts, opts)
    return(opts2)
}

