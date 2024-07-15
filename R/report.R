





tag_element <- function(type) {
    function(x) {
        content <- list("value" = x)
        class(content) <- c(type, "tag_element")
        content
    }
}

tag_h1 <- tag_element("tag_h1")
tag_h2 <- tag_element("tag_h2")
tag_h3 <- tag_element("tag_h3")
tag_h4 <- tag_element("tag_h4")
tag_p <- tag_element("tag_p")
tag_table <- tag_element("tag_table")

tag_br <- function() {
    content <- list("value" = "")
    class(content) <- c("tag_br", "tag_element")
    content
}



report_chunk <- function(..., .order = 9999) {
    assert_that(
        is.numeric(.order),
        length(.order) == 1,
        !is.na(.order)
    )
    content <- list(...)
    assert_that(
        length(content) >= 1,
        all(vapply(content, function(x) is(x, "tag_element"), logical(1)))
    )
    class(content) <- "report_chunk"
    attr(content, "order") <- .order
    content
}

report <- function(...) {
    content <- list(...)
    assert_that(
        length(content) >= 1,
        all(vapply(content, function(x) is(x, "report_chunk"), logical(1)))
    )
    class(content) <- "report"
    content
}


render <- function(x, ...) {
    UseMethod("render", x)
}

render.report <- function(x, ...) {
    content <- vapply(x, render, character(1), ...)
    ord <- vapply(x, function(x) attr(x, "order"), numeric(1))
    content[order(ord)] |>
        paste0(collapse = render(tag_br(), ...))
}

render.report_chunk <- function(x, ...) {
    vapply(x, render, character(1), ...) |>
        paste0(collapse = render(tag_br(), ...))
}

render.tag_element <- function(x, type = "ascii", ...) {
    if (type == "ascii") {
        return(render_ascii(x, ...))
    }
    if (type == "md") {
        return(render_md(x, ...))
    }
    stop("Not implemented TODO")
}


render_ascii <- function(x, ...) {
    UseMethod("render_ascii")
}

render_ascii.tag_table <- function(x, row_limit = 10, ...) {
    if (!is.null(row_limit)) {
        assertthat::assert_that(
            assertthat::is.number(row_limit),
            row_limit > 0,
            msg = "row_limit must be a positive integer"
        )
    }
    if (is.null(row_limit)) {
        display_table <- x$value
    } else {
        display_table <- subset(x$value, seq_len(nrow(x$value)) < (row_limit + 1))
    }

    if (!is.null(row_limit) && nrow(x$value) > row_limit) {
        add_message <- sprintf(
            "First %i of %i rows are shown in table below",
            row_limit,
            nrow(x$value)
        )
    } else {
        add_message <- "All rows are shown in table below"
    }

    paste(
        c(add_message, as_ascii_table(display_table), "\n"),
        collapse = "\n"
    )
}

render_ascii.tag_h1 <- function(x, ...) {
    paste(paste(x$value, collapse = " "), "\n")
}

render_ascii.tag_h2 <- function(x, ...) {
    paste(paste(x$value, collapse = " "), "\n")
}

render_ascii.tag_h3 <- function(x, ...) {
    paste(paste(x$value, collapse = " "), "\n")
}

render_ascii.tag_h4 <- function(x, ...) {
    paste(paste(x$value, collapse = " "), "\n")
}

render_ascii.tag_p <- function(x, ...) {
    paste(paste(x$value, collapse = " "), "\n")
}

render_ascii.tag_br <- function(x, ...) {
    "\n"
}

combine <- function(x, y) {
    if (is(x, "report")) {
        if (is(y, "report")) {
            args <- append(x, y)
            return(do.call(report, args))
        }
        if (is(y, "report_chunk")) {
            args <- append(x, report(y))
            return(do.call(report, args))
        }
        if (is(y, "tag_element")) {
            args <- append(x, report(report_chunk(y)))
            return(do.call(report, args))
        }
    }
    if (is(x, "report_chunk")) {
        if (is(y, "report")) {
            args <- append(report(x), y)
            return(do.call(report, args))
        }
        if (is(y, "report_chunk")) {
            args <- append(x, y)
            args$.order <- attr(x, "order")
            return(do.call(report_chunk, args))
        }
        if (is(y, "tag_element")) {
            args <- append(x, report_chunk(y))
            args$.order <- attr(x, "order")
            return(do.call(report_chunk, args))
        }
    }
    if (is(x, "tag_element")) {
        if (is(y, "report")) {
            args <- append(report(report_chunk(x, .order = 0)), y)
            return(do.call(report, args))
        }
        if (is(y, "report_chunk")) {
            args <- append(report_chunk(x), y)
            args$.order <- attr(y, "order")
            return(do.call(report_chunk, args))
        }
        if (is(y, "tag_element")) {
            args <- list(x, y)
            return(do.call(report_chunk, args))
        }
    }
    stop("Either `x` or `y` are an unsupported type")
}



# x <- combine(
#     report(report_chunk(tag_p("abc"))),
#     report_chunk(tag_p("def"))
# )
# x |> render() |> cat()


# x <- combine(
#     report_chunk("xxx" = tag_p("abc"), .order = 10),
#     report_chunk("xxx" = tag_p("edf"), .order = 10)
# )
# x |> render() |> cat()


# render(
#     report(
#         report_chunk(tag_p("abc"), tag_table(iris), .order = 2),
#         report_chunk(tag_p("This comes first"), tag_table(iris), .order = 1)
#     ),
#     row_limit = 3
# ) |>
#     cat()

# x <- report(
#     "SB" = report_chunk(
#         tag_p("Section B"),
#         tag_table(iris),
#         .order = 10
#     ),
#     report_chunk(tag_h1("Section C")),
#     "SA" = report_chunk(
#         title = tag_p("Section A"),
#         table = tag_table(iris),
#         .order = 2
#     )
# )


# render(x, row_limit = 2) |> cat()

# x[["SB"]] <- combine(
#     x[["SB"]],
#     tag_p("Some wrap up text")
# )


# tag_h1("Some random title") |>
#     combine(x) |>
#     combine(tag_p("Some footer")) |>
#     render(row_limit = 2) |>
#     cat()

