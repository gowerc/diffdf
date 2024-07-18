
test_that("Class comparison works as expected when enabled", {
    expect_output(
        print(diffdf(iris, iris)),
        regexp = "No issues were found"
    )

    expect_warning(
        diffdf(iris, tibble(iris)),
        regexp = "differences between the class"
    )

    expect_warning(
        diffdf(
            tibble(x = 0, z = 3, y = 4, x1 = 5, x2 = 6, x4 = 5),
            data.frame(x = 1, y = 2)
        ),
        regexp = "differences between the class"
    )

    expect_snapshot(
        diffdf(iris, iris)
    )

    expect_snapshot(
        diffdf(tibble(iris), iris, suppress_warnings = TRUE)
    )
})

test_that("Class comparison works as expected when disabled", {
    expect_output(
        print(diffdf(iris, iris, check_df_class = FALSE)),
        regexp = "No issues were found"
    )
    expect_output(
        print(diffdf(tibble(iris), iris, check_df_class = FALSE)),
        regexp = "No issues were found"
    )

    # Check that the "summary table" is still displayed even when the check is disabled
    expect_output(
        diffdf(
            tibble(x = 1, y = 2),
            data.frame(x = 1),
            suppress_warnings = TRUE,
            check_df_class = FALSE
        ) |>
            print(),
        regexp = "Summary of BASE and COMPARE.*There are columns in BASE that are not in COMPARE"
    )
    # Check exact warning message, e.g. are making sure that "Difference in Classes" does
    # not appear if we disable the check
    expect_warning(
        diffdf(
            tibble(x = 1, y = 2),
            data.frame(x = 1),
            check_df_class = FALSE
        ),
        regexp = "^\\nThere are columns in BASE that are not in COMPARE !!$"
    )
})
