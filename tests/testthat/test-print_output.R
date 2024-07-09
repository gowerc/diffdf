





test_that("Print output is maintained", {

    runme <- function(id) {
        x2 <- diffdf(
            list_of_comparisons[[id]][[1]],
            list_of_comparisons[[id]][[2]],
            suppress_warnings = TRUE
        )
        print(x2)
    }

    expect_snapshot(runme("Identical"))
    expect_snapshot(runme("Identical 2"))
    expect_snapshot(runme("Different Values"))
    expect_snapshot(runme("Different Values 2"))
    expect_snapshot(runme("Different attributes"))
    expect_snapshot(runme("Different attributes 2"))
    expect_snapshot(runme("Different Levels"))
    expect_snapshot(runme("Different Levels 2"))
    expect_snapshot(runme("Different Class"))
    expect_snapshot(runme("Different Class 2"))
    expect_snapshot(runme("Different Modes"))
    expect_snapshot(runme("Different Modes 2"))
    expect_snapshot(runme("Missing Columns"))
    expect_snapshot(runme("Missing Columns 2"))
    expect_snapshot(runme("Missing Rows"))
    expect_snapshot(runme("Missing Rows 2"))
    expect_snapshot(runme("everything"))
    expect_snapshot(runme("everything 2"))
    expect_snapshot(runme("Missing Vs NA"))

    expect_snapshot(
        print(
            diffdf(
                list_of_comparisons[["everything"]][[1]],
                list_of_comparisons[["everything"]][[2]],
                keys = "ID",
                suppress_warnings = TRUE
            )
        )
    )

    expect_snapshot(
        print(
            diffdf(
                list_of_comparisons[["everything"]][[1]],
                list_of_comparisons[["everything"]][[2]],
                keys = c("ID", "GROUP1"),
                suppress_warnings = TRUE
            )
        )
    )
})


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
        "row_limit must be a positive integer"

    )
    expect_error(
        print(diff, row_limit = "String"),
        "row_limit must be a positive integer"

    )
    expect_error(
        print(diff, row_limit = NA),
        "row_limit must be a positive integer"

    )
    expect_error(
        print(diff, row_limit = c(1, 2)),
        "row_limit must be a positive integer"

    )

    expect_error(
        print(diff, as_string = "String"),

    )
    expect_error(
        print(diff, as_string = c(TRUE, TRUE)),
    )
})
