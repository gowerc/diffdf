#' Print diffdf objects
#'
#' Print nicely formatted version of an diffdf object
#' @param x comparison object created by diffdf().
#' @param ... Additional arguments (not used)
#' @param row_limit Max row limit for difference tables (NULL to show all rows)
#' @param as_string Return printed message as an R character vector?
#' @examples
#' x <- subset(iris, -Species)
#' x[1, 2] <- 5
#' COMPARE <- diffdf(iris, x)
#' print(COMPARE)
#' print(COMPARE, row_limit = 5)
#' @export
print.diffdf <- function(x, row_limit = 10, as_string = FALSE, ...) {
    if (!is.null(row_limit)) {
        assertthat::assert_that(
            assertthat::is.number(row_limit),
            row_limit > 0,
            msg = "row_limit must be a positive integer"
        )
    }
    assertthat::assert_that(
        assertthat::is.flag(as_string)
    )
    COMPARE <- x

    if (length(COMPARE) == 0) {
        outtext <- "No issues were found!\n"
    } else {
        start_text <- paste0("Differences found between the objects!\n\n")
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
