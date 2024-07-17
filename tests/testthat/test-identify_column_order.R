

test_that("Column order checks work as expected", {
    ## Difference no missing columns
    x1 <- tibble(
        x = 1,
        y = 2,
        z = 3,
        q = 4
    )
    x2 <- tibble(
        x = 1,
        y = 2,
        q = 4,
        z = 3
    )
    actual <- identify_column_order_differences(x1, x2) |> tibble()
    expected <- tibble(
        COLUMN = c("z", "q"),
        "BASE-INDEX" = c(3, 4),
        "COMPARE-INDEX" = c(4, 3)
    )
    expect_equal(actual, expected)
    expect_warning(
        diffdf(x1, x2),
        regex = "the column ordering"
    )
    expect_snapshot(diffdf(x1, x2, suppress_warnings = TRUE))


    ## Difference due to missing column
    x1 <- tibble(
        x = 1,
        y = 2,
        z = 3,
        q = 4
    )
    x2 <- tibble(
        x = 1,
        z = 3,
        q = 4
    )
    actual <- identify_column_order_differences(x1, x2) |> tibble()
    expected <- tibble(
        COLUMN = c("z", "q"),
        "BASE-INDEX" = c(3, 4),
        "COMPARE-INDEX" = c(2, 3)
    )
    expect_equal(actual, expected)
    expect_warning(
        diffdf(x1, x2),
        regex = "the column ordering"
    )
    expect_snapshot(diffdf(x1, x2, suppress_warnings = TRUE))


    ## No-Difference both with missing column
    x1 <- tibble(
        x = 1,
        y1 = 2,
        z = 3,
        q = 4
    )
    x2 <- tibble(
        x = 1,
        y2 = 2,
        z = 3,
        q = 4
    )
    actual <- identify_column_order_differences(x1, x2) |> tibble()
    expected <- tibble(
        COLUMN = character(0),
        "BASE-INDEX" = numeric(0),
        "COMPARE-INDEX" = numeric(0)
    )
    expect_equal(actual, expected)
    expect_snapshot(diffdf(x1, x2, suppress_warnings = TRUE))



    ## No differences
    x1 <- tibble(
        x = 1,
        y = 2,
        z = 3,
        q = 4
    )
    actual <- identify_column_order_differences(x1, x1) |> tibble()
    expected <- tibble(
        COLUMN = character(0),
        "BASE-INDEX" = numeric(0),
        "COMPARE-INDEX" = numeric(0)
    )
    expect_equal(actual, expected)
    expect_warning(
        diffdf(x1, x1),
        regex = NA
    )
})
