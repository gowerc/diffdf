#' Print diffdf objects
#'
#' Print a nicely formatted version of a diffdf object.
#'
#' @param x A comparison object created by \code{diffdf()}.
#' @param ... Additional arguments (not used).
#' @param row_limit Maximum number of rows to display in difference tables.
#' Use \code{NULL} to show all rows. Default is 10.
#' @param as_string Logical. If \code{TRUE}, returns the printed message as an R
#' character vector instead of printing to the console. Default is \code{FALSE}.
#' @param file A connection or a character string naming the file to print to. If
#' \code{NULL} (the default), output is printed to the console.
#'
#' @examples
#' x <- subset(iris, -Species)
#' x[1, 2] <- 5
#' COMPARE <- diffdf(iris, x)
#' print(COMPARE)
#' print(COMPARE, row_limit = 5)
#' \dontrun{
#' print(COMPARE, file = "output.txt")
#' }
#'
#' @export
print.diffdf <- function(x, row_limit = 10, as_string = FALSE, file = NULL, ...) {
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

    string_content <- strsplit(outtext, "\n")[[1]]
    if (!is.null(file)) {
        tryCatch(
            {
                sink(file)
                cat(string_content, sep = "\n")
                sink()
            },
            warning = function(w) {
                sink()
                warning(w)
            },
            error = function(e) {
                sink()
                stop(e)
            }
        )
        return(invisible(COMPARE))
    }

    if (as_string) {
        return(string_content)
    } else {
        cat(outtext)
        return(invisible(COMPARE))
    }
}
