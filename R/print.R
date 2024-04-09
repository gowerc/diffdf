
#' Print diffdf objects
#'
#' Print nicely formatted version of an diffdf object
#' @param x comparison object created by diffdf().
#' @param ... Additional arguments (not used)
#' @param row_limit Max row limit for difference tables (NULL to show all rows)
#' @param as_string Return printed message as an R character vector?
#' @examples
#' x <- subset( iris , -Species )
#' x[1,2] <- 5
#' COMPARE <- diffdf( iris, x)
#' print( COMPARE )
#' print( COMPARE, row_limit = 5 )
#' @export
print.diffdf <- function(x, row_limit = 10, as_string = FALSE, ...) {
    if (!is.null(row_limit)) {
        if (length(row_limit) != 1) {
            stop("row_limit should have a length of 1")
        }
        if (!is.numeric(row_limit)) {
            stop("row_limit should be a numeric value or NULL")
        }
        if (row_limit <= 0) {
            stop("row_limit should be a positive integer")
        }
    }
    if (!is.logical(as_string) || length(as_string) != 1) {
        stop("as_string should be a logical of length one")
    }
    COMPARE <- x

    if (length(COMPARE) == 0) {
        outtext <- "No issues were found!\n"
    } else {
        start_text <- paste0(
            "Differences found between the objects!\n\n",
            "A summary is given below.\n\n"
        )
        end_text <- lapply(COMPARE, function(x) get_print_message(x, row_limit))
        end_text <- paste0(unlist(end_text), collapse = "")
        outtext <- paste0(start_text, end_text)
    }
    if (as_string) {
        return(strsplit(outtext, "\n")[[1]])
    } else {
        cat(outtext)
        return(invisible(COMPARE))
    }
}
