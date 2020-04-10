get_issue_message <- function (object, ...) {
    UseMethod("get_issue_message", object)
}

get_issue_message.default <- function(object, ...){
    return(NULL)
}



get_issue_value <- function (object, ...) {
    UseMethod("get_issue_value", object)
}

get_issue_value.default <- function(object, ...){
    return(NULL)
}
