

#' diffopts
#'
#' Options to modify how diffdf performs its comparison
#' @param tolerance Set tolerance for numeric comparisons. Note that comparisons fail if (x-y)/scale > tolerance
#' @param scale Set scale for numeric comparisons. Note that comparisons fail if (x-y)/scale > tolerance 
#' @param onfailure How should messages be raised if a comparison fails. Options are "warning", "message", "error" & "nothing". Default = "warning"
#' @param ... Not used but included for compatibility reasons
#'  
#' @export
diffopts <- function(
    scale = 1,
    tolerance = sqrt(.Machine$double.eps),
    onfailure = "warning",
    ...
){
    
    dots <- list(...)
    
    if( !is.null(dots$suppress_warnings)){
        stop( "`suppress_warnings = TRUE` is depreciated as of diffdf v2.0.0\nPlease use `onfailure = 'nothing'` instead" )
    }
    
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
#' Merges a named list of options with named arguments via dots
#' @param opts List of named options
#' @param ... named arguments
merge_options <- function(opts = NULL, ...){
    dots <- list(...)
    if( is.null(opts)) opts <- list()
    for( i in names(dots)) opts[[i]] <- dots[[i]]
    opts2 <- do.call(diffopts, opts)
    return(opts2)
}

