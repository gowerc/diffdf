

construct_check <- function(
    name = "", 
    value,
    is_test = TRUE,
    print_me = was_performed & !is_pass, 
    was_performed = TRUE,
    error_message = "", 
    is_pass = nrow(value) == 0, 
    ord = 999
){
    
    if( ! any( class(value) %in%  c("data.frame", "tbl", "data.frame"))){
        stop("Issue value is not a dataframe object")
    }
    
    x <- list(
        name = name,
        was_performed = was_performed,
        print_me = print_me,
        is_pass = is_pass, 
        value = value,
        is_test = is_test,
        error_message = error_message,
        ord = ord
    )
    class(x) <- "check"
    return(x)
}



get_name.check <- function(x, ...){
    return( x$name)
}

get_print_me.check <- function(x, ...){
    return( x$print_me)
}

get_was_performed.check <- function(x, ...){
    return( x$was_performed)
}

get_is_pass.check <- function(x, ...){
    return( x$is_pass)
}

get_value.check <- function(x, ...){
    return( x$value)
}

get_error_message.check <- function(x, ...) {
    return( x$error_message )  
}

get_ord.check <- function(x, ...){
    return( x$ord)
}

get_is_test.check <- function(x, ...){
    return( x$is_test)
}











