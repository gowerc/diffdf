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

error_generate <- function(conditions, message){
    if(!conditions){
        stop(
            message,
            .call = FALSE
        )
    }
}


options_list <-
    list(
        warnings = function(value) {
            error_generate(
                is.logical(value) & length(value) == 1,
                "Option warnings is not a logical or is not of length 1"
            )
            },
        strict_numeric = function(value) { 
            error_generate(
                is.logical(value) & length(value) == 1,
                "Option strict_numeric is not a logical or is not of length 1"
        )
        },
        strict_factor = function(value) { 
            error_generate(
                is.logical(value) * length(value) == 1,
                "Option strict_factor is not a logical or is not of length 1"
            )
        },
        file  = function(value) { 
            error_generate(
                is.null(value)| is.character(value) & length(value) == 1,
                "Option file is not NULL or a string of length 1"
            )
        },
        tolerance = function(value) { 
            error_generate(
                is.numeric(value) & length(value) == 1,
                "Option tolerance is not a numeric of length 1"
            )
        },
        scale = function(value) { 
            error_generate(
                is.null(value)| is.numeric(value) & length(value) == 1,
                "Option scale is not NULL or a numeric of length 1"
            )
        }
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
#' @param warnings  Should diffdf display warnings when it finds issues? (default = getOption("diffdf_options")$warnings)
#' @param strict_numeric  Flag for strict numeric to numeric comparisons (default = getOption("diffdf_options")$strict_numeric). 
#'  If False diffdf will cast integer to double where required for comparisons. 
#'  Note that variables specified in the keys will never be casted.
#' @param strict_factor  Flag for strict factor to character comparisons (default = getOption("diffdf_options")$strict_factor). 
#'  If False diffdf will cast factors to characters where required for comparisons. 
#' @param file  Location and name of a text file to output the results to. 
#'  Setting to NULL will cause no file to be produced. (Default = getOption("diffdf_options")$file)
#' @param tolerance  Set tolerance for numeric comparisons. Note that comparisons fail if (x-y)/scale > tolerance. (default = getOption("diffdf_options")$tolerance)
#' @param scale Set scale for numeric comparisons. Note that comparisons fail if (x-y)/scale > tolerance.
#'   Setting as NULL is a slightly more efficient version of scale = 1. (default = getOption("diffdf_options")$scale)
#' @return invisibly returns options
#' @examples  
#' 
#' diffdf_options(warnings = FALSE)
#' x <- subset(iris,  -Species)
#' x[1,2] <- 5
#' COMPARE <- diffdf(iris, x)
#' 
diffdf_options <- function(warnings = getOption("diffdf_options")$warnings,
                           strict_numeric = getOption("diffdf_options")$strict_numeric,
                           strict_factor = getOption("diffdf_options")$strict_factor,
                           file = getOption("diffdf_options")$file,
                           tolerance = getOption("diffdf_options")$tolerance,
                           scale = getOption("diffdf_options")$scale
                           ){
    
    new_options <- list(
        warnings = warnings,
        strict_numeric = strict_numeric,
        strict_factor = strict_factor,
        file = file,
        tolerance = tolerance,
        scale = scale
    )
    
    options_checks(new_options)
    
    options("diffdf_options" =  new_options)
    
    invisible(new_options)
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


