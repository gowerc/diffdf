

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
        diffdf(x1, x2, check_column_order = TRUE),
        regex = "the column ordering"
    )
    expect_snapshot(diffdf(x1, x2, suppress_warnings = TRUE, check_column_order = TRUE))


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
        diffdf(x1, x2, check_column_order = TRUE),
        regex = "the column ordering"
    )
    expect_snapshot(diffdf(x1, x2, suppress_warnings = TRUE, check_column_order = TRUE))


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
    expect_snapshot(diffdf(x1, x2, suppress_warnings = TRUE, check_column_order = TRUE))



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
        diffdf(x1, x1, check_column_order = TRUE),
        regex = NA
    )
})


test_that("Column order checks work with null columns", {
    d1 <- data.frame(
        x = numeric(0),
        y = numeric(0),
        z = numeric(0)
    )
    d2 <- data.frame(
        x = numeric(0),
        z = numeric(0),
        y = numeric(0)
    )
    actual <- identify_column_order_differences(d1, d2) |> tibble()
    expected <- tibble(
        COLUMN = c("y", "z"),
        "BASE-INDEX" = c(2, 3),
        "COMPARE-INDEX" = c(3, 2)
    )
    expect_equal(actual, expected)
    expect_warning(
        diffdf(d1, d2, check_column_order = TRUE),
        regex = "the column ordering"
    )
    expect_snapshot(diffdf(d1, d2, suppress_warnings = TRUE, check_column_order = TRUE))
})


test_that("By default column orders are not checked", {
    d1 <- data.frame(
        x = numeric(0),
        y = numeric(0),
        z = numeric(0)
    )
    d2 <- data.frame(
        x = numeric(0),
        z = numeric(0),
        y = numeric(0)
    )
    expect_warning(
        diffdf(d1, d2),
        regex = NA
    )
    expect_snapshot(diffdf(d1, d2, suppress_warnings = TRUE))
})

test_that("Edge cases that once caused bugs now work as expected", {
    x1 <- data.frame(x = 1)
    expect_no_error(
        diffdf(x1, x1)
    )
})


test_that("Coloumn order checks work with keys", {
    d1 <- tibble(
        x = c(1, 2, 3),
        id = c("A", "B", "C"),
        id2 = c("A1", "B2", "C3"),
        y = c(4, 5, 6),
        z = c(7, 8, 9)
    )
    d2 <- tibble(
        id = c("A", "B", "C"),
        id3 = c("A", "B", "C"),
        x = c(1, 2, 3),
        y = c(4, 5, 6),
        z = c(7, 8, 9)
    )
    actual <- identify_column_order_differences(d1, d2) |> tibble()
    expected <- tibble(
        COLUMN = c("x", "id"),
        "BASE-INDEX" = c(1, 2),
        "COMPARE-INDEX" = c(3, 1)
    )
    expect_equal(actual, expected)
    expect_warning(
        diffdf(d1, d2, keys = c("id"), check_column_order = TRUE),
        regex = "the column ordering"
    )
})
