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
        file  = function(value) {stopifnot(is.null(value)| is.character(value) & length(value) == 1)},
        tolerance = function(value) {stopifnot(is.numeric(value), length(value) == 1)},
        scale = function(value) {stopifnot(is.null(value)| is.numeric(value) & length(value) == 1)}
    )

options_default <-
    list(
        warnings = TRUE, 
        strict_numeric = TRUE,
        strict_factor = TRUE,
        file = NULL,
        tolerance = sqrt(.Machine$double.eps),
        scale = NULL
    )


options_checks <- function(options){
    
    if(!is.list(options)){
        stop("Options should be a list")
    }
    
    if(any(
        c(!options_list$names %in% names(options))
    )){
        stop("Options are missing from options(\"diffdf_options\"). Please run diffdf_options(reset = TRUE) to reset")
    }  
    options <- options[names(options_list)]
    
    Map(error_check_options, funin = options_list, value = options)
    invisible()
}



#' diffdf_options
#' 
#' Set options for diffdf. Will return the options, so can be called directly by diffdf. 
#' @export
#' @param warnings \code{(logical)} Should diffdf display warnings when it finds issues? (default = TRUE)
#' @param strict_numeric Flag for strict numeric to numeric comparisons (default = TRUE). 
#'  If False diffdf will cast integer to double where required for comparisons. 
#'  Note that variables specified in the keys will never be casted.
#' @param strict_factor Flag for strict factor to character comparisons (default = TRUE). 
#'  If False diffdf will cast factors to characters where required for comparisons. 
#'  Note that variables specified in the keys will never be casted.
#' @param warnings Do you want to display warnings? (logical) (default = TRUE)
#' @param file Location and name of a text file to output the results to. 
#'  Setting to NULL will cause no file to be produced. (Default = NULL)
#' @param tolerance Set tolerance for numeric comparisons. Note that comparisons fail if (x-y)/scale > tolerance. (default = sqrt(.Machine$double.eps))
#' @param scale Set scale for numeric comparisons. Note that comparisons fail if (x-y)/scale > tolerance.
#'   Setting as NULL is a slightly more efficient version of scale = 1. (default = NULL)
#' @return invisibly returns options
#' @examples  
#' 
#' diffdf_options(warnings = FALSE)
#' x <- subset(iris,  -Species)
#' x[1,2] <- 5
#' COMPARE <- diffdf(iris, x)
#' 
diffdf_options <- function(...){
    
    current_options <- options("diffdf_options")[[1]]
    
    args <- list(...)
    
    if(length(args) == 0){
        options_checks(current_options)
        return(current_options)
    }
    
    namecheck <- names(args) %in% names(options_list)
    if(!all(namecheck )){
        warning(
        paste0("The following provided options are not included in diffdf, and will be ignored ",
               paste(names(args)[!namecheck], collapse = " "))
        )
    }
    reduced_names <- names(args)[namecheck]
    
    if(length(reduced_names) == 0){
        options_checks(current_options)
        return(current_options)
    }
    
    current_options[reduced_names] <- args[reduced_names]
    
    options_checks( current_options)
    
    options("diffdf_options" =  current_options)
    
    invisible(current_options)
}

#' diffdf_options_reset
#' 
#' resets all diffdf options to default
#' @return invisibly returns default options
#' @export  
#' @examples
#' diffdf_options(warnings = FALSE)
#' diffdf_options_reset()
#' x <- subset(iris,  -Species)
#' x[1,2] <- 5
#' COMPARE <- diffdf(iris, x)
#' 
diffdf_options_reset <- function(){
    diffdf_options(warnings = TRUE, 
                   strict_numeric = TRUE,
                   strict_factor = TRUE,
                   file = NULL,
                   tolerance = sqrt(.Machine$double.eps),
                   scale = NULL)
}


