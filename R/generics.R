


get_name <- function(x, ...){
    UseMethod("get_name", x)
}

get_was_performed <- function(x, ...){
    UseMethod("get_was_performed", x)
}

get_is_pass <- function(x, ...){
    UseMethod("get_is_pass", x)
}

get_value <- function(x, ...){
    UseMethod("get_value", x)
}

get_error_message <- function(x, ...) {
    UseMethod("get_error_message", x)
}

get_ord <- function(x, ...){
    UseMethod("get_ord", x)
}

get_collection <- function(x,...){
    UseMethod("get_collection", x)
}

get_print_me <- function(x,...){
    UseMethod("get_print_me", x)
}

get_is_test <- function(x,...){
    UseMethod("get_is_test", x)
}



