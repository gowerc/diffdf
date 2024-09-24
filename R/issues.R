#' construct_issue
#'
#' Make an s3 object with class issue and possible additional class,
#' and assign other arguments to attributes
#' @param value the value of the object
#' @param message the value of the message attribute
#' @param add_class additional class to add
#' @keywords internal
construct_issue <- function(value, message, add_class = NULL) {
    x <- value

    ### If nothing has been provided return nothing !
    if (nrow(x) == 0) {
        return(NULL)
    }

    class(x) <- c(add_class, "issue", class(x))
    attributes(x)[["message"]] <- message
    return(x)
}


#' get_issue_message
#'
#' Simple function to grab the issue message
#' @param object inputted object of class issue
#' @param ... other arguments
#' @keywords internal
get_issue_message <- function(object, ...) {
    return(attr(object, "message"))
}


#' get_print_message
#'
#' Get the required text depending on type of issue
#' @param object inputted object of class issue
#' @param ... other arguments
#' @keywords internal
get_print_message <- function(object, ...) {
    UseMethod("get_print_message", object)
}


#' get_print_message.default
#'
#' Errors, as this should only ever be given an issue
#' @param object issue
#' @param ... Not used
#' @keywords internal
get_print_message.default <- function(object, ...) {
    stop("Error: An issue has not been provided to this function!")
}


#' get_print_message.issue
#'
#' Get text from a basic issue, based on the class of the value of the issue
#'
#' @param object an object of class issue_basic
#' @inheritParams print.diffdf
#' @keywords internal
get_print_message.issue <- function(object, row_limit, ...) {
    paste(
        c(
            attr(object, "message"),
            get_table(object, row_limit = row_limit),
            "\n\n"
        ),
        collapse = "\n"
    )
}
