

construct_issue_collection <- function(issues, message = "", order = 999, add_class = NULL){
   
    cls <- vapply( issues, class, character(1))
    stopifnot(
        length(issues) > 0,
        all(cls == "issue")
    )
    
    x <- list(
        value = issues,
        message = message,
        ord = order
    )
    class(x) <- c(add_class, "issue_collection")
    return(x)
}



#' get_issue_message
#' 
#' Simple function to grab the issue message
#' @param object inputted object of class issue
#' @param ... other arguments
get_issue_message.issue_collection <- function(object, ...) {
    return( object$message )  
}

get_issue_value.issue_collection <- function(object, ...){
    return( object$value)
}



