#' error_check_options
#' 
#' Executes check of value for given function
#' @param funin Function to check with
#' @param value Value to check meets expectations
#' @return NULL
error_check_options <- function(funin, value){
    funin(value)
    invisible()
}


options_list <-
    list(
        warnings = function(value) {stopifnot(is.logical(value), length(value) == 1)},
        strict_numeric = function(value) {stopifnot(is.logical(value), length(value) == 1)},
        strict_factor = function(value) {stopifnot(is.logical(value), length(value) == 1)},
        file  = function(value) {stopifnot(is.null(value)| is.numeric(value) & length(value) == 1)},
        tolerance = function(value) {stopifnot(is.numeric(value), length(value) == 1)},
        scale = function(value) {stopifnot(is.null(value)| is.numeric(value) & length(value) == 1)}
    )


options_checks <- function(options){
    
    if(!is.list(options)){
        stop("Options should be a list")
    }
    
    if(any(
        c(!options_list$names %in% names(options))
    )){
        stop("Options are missing from options(\"diffdf_options\"). Please run diffdf_set_options() to reset")
    }  
    options <- options[names(options_list)]
    
    Map(error_check_options, funin = options_list, value = options)
    invisible()
}



#' diffdf_set_options
#' 
#' Set the options for use in \code{\link{diffdf}}. These are called in \code{diffdf(config = get_options())}.
#' Options which are not expressly given will be set to their default value by this function
#' @export
#' @param warnings \code{(logical)} Should diffdf display warnings when it finds issues?
#' @param strict_numeric Flag for strict numeric to numeric comparisons (default = TRUE). 
#'  If False diffdf will cast integer to double where required for comparisons. 
#'  Note that variables specified in the keys will never be casted.
#' @param strict_factor Flag for strict factor to character comparisons (default = TRUE). 
#'  If False diffdf will cast factors to characters where required for comparisons. 
#'  Note that variables specified in the keys will never be casted.
#' @param warnings Do you want to display warnings? (logical) (default = TRUE)
#' @param file Location and name of a text file to output the results to. 
#'  Setting to NULL will cause no file to be produced.
#' @param tolerance Set tolerance for numeric comparisons. Note that comparisons fail if (x-y)/scale > tolerance.
#' @param scale Set scale for numeric comparisons. Note that comparisons fail if (x-y)/scale > tolerance.
#'   Setting as NULL is a slightly more efficient version of scale = 1. 
#' @return diffdf options updated, and NULL returned
#' @examples  
#' 
#' diffdf_set_options(warnings = FALSE)
#' #' x <- subset(iris,  -Species)
#' x[1,2] <- 5
#' COMPARE <- diffdf(iris, x)
#' 
diffdf_set_options <- function(warnings = TRUE, 
                               strict_numeric = TRUE,
                               strict_factor = TRUE,
                               file = NULL,
                               tolerance = sqrt(.Machine$double.eps),
                               scale = NULL){
    
    new_options <- list(
        warnings = warnings, 
        strict_numeric =  strict_numeric,
        strict_factor = strict_factor,
        file = file,
        tolerance = tolerance ,
        scale = scale
    )
    
    options_checks(new_options)
    
    options("diffdf_options" = new_options)
    
    invisible()
}






#' diffdf_get_options
#' 
#' Get the options for use in \code{\link{diffdf}}.  
#' @export
#' @return list of current diffdf options
#' @examples  
#' 
#' diffdf_set_options(warnings = FALSE)
#' #' x <- subset(iris,  -Species)
#' x[1,2] <- 5
#' COMPARE <- diffdf(iris, x)
#' 
diffdf_get_options <- function(){
    
    current_options <- options("diffdf_options")[[1]]
    
    tryCatch(
        options_checks(current_options),
        error = function(e){
            stop(
                paste0("diffdf options have become malformed,", 
                       "probably due to direct use of Options(). Use diffdf_set_options()",
                       "to reset to default"),
                call. = FALSE
            )
        }
    )
    
    current_options
    
}