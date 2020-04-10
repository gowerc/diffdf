#' construct_issue
#' 
#' Make an s3 object with class issue and possible additional class,
#' and assign other arguments to attributes
#' @param value the value of the object
#' @param message the value of the message attribute
#' @param add_class additional class to add
construct_issue <- function(value, message = "", order = 999, add_class = NULL){
    
    if( ! any( class(value) %in%  c("data.frame", "tbl", "data.frame"))){
        stop("Issue value is not a dataframe object")
    }
    
    if(nrow(value) == 0 ) return(NULL)
    
    x <- list(
        value = value,
        message = message,
        ord = order
    )
    class(x) <- c(add_class, "issue")
    return(x)
}




#' get_issue_message
#' 
#' Simple function to grab the issue message
#' @param object inputted object of class issue
#' @param ... other arguments
get_issue_message <- function(object, ...) {
    return( object$message )  
}

get_issue_value <- function(object, ...){
    return( object$value)
}


#' get_print_message
#' 
#' Get the required text depending on type of issue
#' @param object inputted object of class issue
#' @param ... other arguments
get_print_message <- function (object, ...) {
    UseMethod("get_print_message", object)
}


#' get_print_message.default
#' 
#' Errors, as this should only ever be given an issue
#' @param object issue
get_print_message.default <- function(object) {
    stop("Error: An issue has not been provided to this function!")
}


#' get_print_message.issue
#'
#' Get text from a basic issue, based on the class of the value of the issue
#'
#' @param object an object of class issue_basic
get_print_message.issue <- function(object){
    paste(
        c( get_issue_message(object),  get_table( get_issue_value(object))),
        collapse = '\n'
    )
}










