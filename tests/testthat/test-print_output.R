context("Testing print functionality")



runme <- function(x) {
    x2 <- diffdf(x[[1]], x[[2]], suppress_warnings = TRUE)
    print(x2, as_string = TRUE)
}

RES <- map(list_of_comparisons, runme)


### Add additional examples that make use of keys

x <- diffdf(
    list_of_comparisons[["everything"]][[1]],
    list_of_comparisons[["everything"]][[2]],
    keys = "ID",
    suppress_warnings = TRUE
)
RES[["With 1 key"]] <- print(x, as_string = TRUE)


x <- diffdf(
    list_of_comparisons[["everything"]][[1]],
    list_of_comparisons[["everything"]][[2]],
    keys = c("ID", "GROUP1"),
    suppress_warnings = TRUE
)
RES[["With 2 keys"]] <- print(x, as_string = TRUE)




SET_GOLD <- FALSE

if (SET_GOLD) {
    TESTING_print_msg <- RES
    devtools::use_data(TESTING_print_msg, internal = TRUE, overwrite = TRUE)
} else {
    for (i in seq_along(RES)) {
        expect_equal(
            RES[[i]],
            TESTING_print_msg[[i]],
            info = paste0("Reference = ", i, " - ", names(RES)[i])
        )
    }
}

test_that("row_limit works as expected", {
    diff <- diffdf(
        data.frame(col1 = LETTERS, col2 = 1:26),
        data.frame(col1 = LETTERS, col2 = 21:46),
        keys = "col1",
        suppress_warnings = TRUE
    )
    output <- print(diff, as_string = TRUE)
    expect_length(output, 10 + 21)
    output <- print(diff, as_string = TRUE, row_limit = 26)
    expect_length(output, 26 + 21)
    output <- print(diff, as_string = TRUE, row_limit = 5)
    expect_length(output, 5 + 21)
    output <- print(diff, as_string = TRUE, row_limit = NULL)
    expect_length(output, 26 + 21)
})

test_that("print.diffdf errors when given bad inputs", {
    diff <- diffdf(
        data.frame(col1 = LETTERS, col2 = 1:26),
        data.frame(col1 = LETTERS, col2 = 21:46),
        keys = "col1",
        suppress_warnings = TRUE
    )
    expect_error(
        print(diff, row_limit = 0),

    )
    expect_error(
        print(diff, row_limit = "String"),

    )
    expect_error(
        print(diff, row_limit = NA),

    )
    expect_error(
        print(diff, row_limit = c(1, 2)),

    )

    expect_error(
        print(diff, as_string = "String"),

    )
    expect_error(
        print(diff, as_string = c(TRUE, TRUE)),
    )
})
