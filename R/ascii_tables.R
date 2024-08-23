#' Pad String
#'
#' Utility function used to replicate `str_pad`. Adds white space to either end
#' of a string to get it to equal the desired length
#' @param x string
#' @param width desired length
#' @keywords internal
string_pad <- function(x, width) {
    if (nchar(x) >= width) {
        return(x)
    }
    width <- width - nchar(x)
    left <- paste0(rep(" ", floor(width / 2)), collapse = "")
    right <- paste0(rep(" ", ceiling(width / 2)), collapse = "")
    paste0(left, x, right, collapse = "")
}


#' recursive_reduce
#'
#' Utility function used to replicated `purrr::reduce`. Recursively applies a
#' function to a list of elements until only 1 element remains
#' @param .l list of values to apply a function to
#' @param .f function to apply to each each element of the list in turn. See details.
#' @details
#' This function is essentially performing the following operation:
#' ```
#' .l[[1]] <- .f( .l[[1]] , .l[[2]]) ; .l[[1]] <- .f( .l[[1]] , .l[[3]])
#' ```
#' @keywords internal
recursive_reduce <- function(.l, .f) {
    if (length(.l) != 1) {
        .l[[2]] <- .f(.l[[1]], .l[[2]])
        return(recursive_reduce(.l[-1], .f))
    } else {
        return(.l[[1]])
    }
}

#' invert
#'
#' Utility function used to replicated `purrr::transpose`. Turns a list inside
#' out.
#' @param x list
#' @keywords internal
invert <- function(x) {
    x2 <- list()
    cnames <- names(x)
    tnames <- names(x[[1]])
    for (i in tnames) {
        x2[[i]] <- list()
        for (j in cnames) {
            x2[[i]][[j]] <- x[[j]][[i]]
        }
    }
    return(x2)
}




#' as_ascii_table
#'
#' This function takes a `data.frame` and attempts to convert it into
#' a simple ascii format suitable for printing to the screen
#' It is assumed all variable values have a `as.character()` method
#' in order to cast them to character.
#' @param dat Input dataset to convert into a ascii table
#' @param line_prefix Symbols to prefix in front of every line of the table
#' @keywords internal
as_ascii_table <- function(dat, line_prefix = "  ") {
    n_col <- ncol(dat)
    n_row <- nrow(dat)

    ## Convert every value to character and crop to a suitable length
    dat_char <- lapply(dat, as_fmt_char)


    hold <- list()
    COLS <- colnames(dat)

    ### For each column extract core elements (width, values , title) and pad out
    ### each string to be a suitable length
    for (i in seq_len(n_col)) {
        COL <- COLS[i]
        VALUES <- dat_char[[i]]

        JOINT <- c(COL, VALUES)
        WIDTH <- max(sapply(JOINT, nchar)) + 2

        hold[[COL]] <- list()
        hold[[COL]]$WIDTH <- WIDTH
        hold[[COL]]$VALUES <- sapply(VALUES, string_pad, width = WIDTH)
        hold[[COL]]$HEADER <- sapply(COL, string_pad, width = WIDTH)
    }

    ### Collapse into a single value per component ( title , values, width )
    thold <- invert(hold)
    tvals <- recursive_reduce(thold$VALUES, paste0)
    thead <- recursive_reduce(thold$HEADER, paste0)
    twidth <- recursive_reduce(thold$WIDTH, sum)

    ### Create header and footer lines
    TLINE <- paste0(rep("=", twidth), collapse = "")
    LINE <- paste0(rep("-", twidth), collapse = "")
    FVALS <- paste0(line_prefix, tvals, collapse = "\n")

    ### Output table
    paste0(
        line_prefix, TLINE, "\n",
        line_prefix, thead, "\n",
        line_prefix, LINE, "\n",
        FVALS, "\n",
        line_prefix, LINE
    )
}


#' as_character
#'
#' Stub function to enable mocking in unit tests
as_character <- as.character

#' Format vector to printable string
#'
#' Coerces a vector of any type into a printable string. The most
#' significant transformation is performed on existing character
#' vectors which will be truncated, have newlines converted
#' to explicit symbols and will be wrapped in quotes if they
#' contain white space.
#'
#' @param x (`vector`) \cr vector to be converted to character
#' @param add_quotes (`logical`) \cr if true will wrap strings that contain
#' whitespace with quotes
#' @param crop_at (`numeric`) \cr specifies the limit at which strings should
#' be truncated to
#' @param ... additional arguments (not currently used)
#'
#' @name as_fmt_char
#' @keywords internal
as_fmt_char <- function(x, ...) {
    UseMethod("as_fmt_char")
}

#' @rdname as_fmt_char
#' @export
as_fmt_char.numeric <- function(x, ...) {
    format(x, digits = 7, justify = "right")
}

#' @rdname as_fmt_char
#' @export
as_fmt_char.NULL <- function(x, ...) {
    "<NULL>"
}

#' @importFrom utils capture.output
#' @rdname as_fmt_char
#' @export
as_fmt_char.list <- function(x, ...) {
    vapply(
        x,
        function(x) {
            if (is.numeric(x)) {
                return(as_fmt_char(x))
            }
            if (is.character(x) & length(x) == 1) {
                return(as_fmt_char(x))
            }
            as_fmt_char(
                paste(capture.output(dput(x)), collapse = " "),
                add_quotes = FALSE
            )
        },
        character(1)
    )
}

#' @rdname as_fmt_char
#' @export
as_fmt_char.factor <- function(x, ...) {
    as_fmt_char(as.character(x))
}

#' @rdname as_fmt_char
#' @export
as_fmt_char.character <- function(x, add_quotes = TRUE, crop_at = 30, ...) {
    needs_quotes <- grepl("\\s", x) & add_quotes

    x[is.na(x)] <- "<NA>"

    # Replace \nl \cr with tags to stop print message splitting over
    # multiple lines
    x <- gsub("\x0D", "<cr>", x)
    x <- gsub("\x0A", "<nl>", x)

    charlength <- vapply(x, nchar, numeric(1))
    x <- substr(x, 1, crop_at)
    x[charlength > crop_at] <- paste0(x[charlength > crop_at], "...")

    # Add enclosing " " around strings with white space so that it can be
    # clearly identified in the printed output
    x[needs_quotes] <- paste0('"', x[needs_quotes], '"')

    return(x)
}


#' @rdname as_fmt_char
#' @export
as_fmt_char.default <- function(x, ...) {
    x_char <- as_character(x)
    assertthat::assert_that(
        is.character(x_char),
        msg = sprintf(
            "Unable to convert class `'%s'` to character for printing purposes",
            paste(class(x), collapse = "', '")
        )
    )
    as_fmt_char.character(x_char, add_quotes = FALSE)
}


#' @rdname as_fmt_char
#' @export
as_fmt_char.POSIXt <- function(x, ...) {
    format(x, "%Y-%m-%d %H:%M:%S %Z")
}


#' get_table
#'
#' Generate nice looking table from a data frame
#' @param dsin dataset
#' @inheritParams print.diffdf
#' @keywords internal
get_table <- function(dsin, row_limit = 10) {
    if (nrow(dsin) == 0) {
        return("")
    }
    if (!is.null(row_limit)) {
        assertthat::assert_that(
            assertthat::is.number(row_limit),
            row_limit > 0,
            msg = "row_limit must be a positive integer"
        )
    }
    if (is.null(row_limit)) {
        display_table <- dsin
    } else {
        display_table <- subset(dsin, seq_len(nrow(dsin)) < (row_limit + 1))
    }

    add_message <- if (!is.null(row_limit) && nrow(dsin) > row_limit) {
        paste0(
            "First ",
            row_limit,
            " of ",
            nrow(dsin),
            " rows are shown in table below"
        )
    } else {
        NULL
    }

    msg <- paste(
        c(
            add_message,
            as_ascii_table(display_table)
        ),
        collapse = "\n"
    )
    return(msg)
}
