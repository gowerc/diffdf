

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
    output_5 <- print(diff, as_string = TRUE, row_limit = 5)
    output_10 <- print(diff, as_string = TRUE, row_limit = 10)
    output_15 <- print(diff, as_string = TRUE, row_limit = 15)
    output_26 <- print(diff, as_string = TRUE, row_limit = 26)
    output_99 <- print(diff, as_string = TRUE, row_limit = 99)
    output_null <- print(diff, as_string = TRUE, row_limit = NULL)

    expect_equal(output, output_10)
    expect_equal(output_26, output_99)
    expect_equal(output_26, output_null)

    # +16 for the difference in the number of rows
    # -1 for the lack of "x of y rows displayed"
    expect_equal(length(output_10) + 16 - 1, length(output_26))
    expect_equal(length(output_10) - 5, length(output_5))
    expect_equal(length(output_10) + 5, length(output_15))
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


test_that("#135 - Writing to file works as expected with row limits", {
    f1 <- withr::local_tempfile()
    f2 <- withr::local_tempfile()

    x2 <- diffdf(
        list_of_comparisons[[5]][[1]],
        list_of_comparisons[[5]][[2]],
        suppress_warnings = TRUE,
        file = f1
    )

    expect_equal(
        readLines(f1),
        print(x2, as_string = TRUE)
    )

    print(x2, file = f2)
    expect_equal(
        readLines(f1),
        readLines(f2)
    )



    f3 <- withr::local_tempfile()

    d1 <- tibble(id = seq_len(30), x = 1)
    d2 <- tibble(id = seq_len(30), x = 0)

    x2 <- diffdf(d1, d2, suppress_warnings = TRUE)
    print(x2, file = f3, row_limit = 5)

    expect_equal(
        readLines(f3),
        print(x2, as_string = TRUE, row_limit = 5)
    )

})
