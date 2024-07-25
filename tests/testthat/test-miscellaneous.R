

test_that("sort order is as expected (sorted by keys)", {
    dsin1 <- data.frame(
        G1 = rep(c(1, 10, 2, 101), 3),
        G2 = as.Date(c(
            "2015-10-12", "2015-10-12", "2015-10-12", "2015-10-12",
            "2015-11-12", "2015-11-12", "2015-11-12", "2015-11-12",
            "2015-12-12", "2015-12-12", "2015-12-12", "2015-12-12"
        )),
        var = c(9, 2, 5, 2, 3, 10, 12, 42, 1, 2, 8, 54)
    )
    dsin2 <- dsin1
    dsin2$var[c(1, 2, 3)] <- c(97, 98, 99)

    x <- diffdf(dsin1, dsin2, keys = c("G1", "G2"), suppress_warnings = TRUE)
    x2 <- diffdf_issuerows(dsin2, x)

    expect_true(all(x$VarDiff_var$G1 == c(1, 2, 10)))
    expect_true(all(x$VarDiff_var$COMPARE == c(97, 99, 98)))
    expect_true(all(x2$G1 == c(1, 2, 10)))
    expect_true(all(x2$var == c(97, 99, 98)))
})



test_that("as_ascii_table() can handle white space and newlines", {
    x1 <- data.frame(
        x = c(
            "no-quotes",
            "yes have\x0D\nquotes",
            "yes it should have quotes but its long"
        )
    )

    gold <- c(
        "",
        "  =====================================",
        "                    x                  ",
        "  -------------------------------------",
        "                no-quotes              ",
        '        "yes have<cr><nl>quotes"       ',
        '   "yes it should have quotes but ..." ',
        "  -------------------------------------"
    )

    expect_equal(
        (as_ascii_table(x1) |> strsplit("\n"))[[1]],
        gold
    )


    #
    # Show that converting to factor makes no difference
    #
    x2 <- x1
    x2$x <- factor(x2$x)
    expect_equal(
        (as_ascii_table(x2) |> strsplit("\n"))[[1]],
        gold
    )
})

test_that("can handle null datasets", {
    expect_warning(
        {res <- diffdf(data.frame(), iris)},
        regexp = "rows in COMPARE that are not in BASE.*columns in COMPARE that are not in BASE"
    )
    expect_snapshot(res)


    expect_warning(
        {res <- diffdf(iris, data.frame())},
        regexp = "rows in BASE that are not in COMPARE.*columns in BASE that are not in COMPARE"
    )
    expect_snapshot(res)


    expect_output(
        print(diffdf(data.frame(), data.frame())),
        "No issues were found!"
    )

    expect_output(
        print(diffdf(iris, iris)),
        "No issues were found!"
    )

    x1 <- tibble(
        x = c(1, 2, 3),
        y = c(4, 5, 6)
    )
    expect_warning(
        {res <- diffdf(x1, x1[FALSE, "x"])},
        regexp = "rows in BASE that are not in COMPARE.*columns in BASE that are not in COMPARE"
    )
    expect_snapshot(res)
    expect_warning(
        {res <- diffdf(x1[FALSE, "x"], x1)},
        regexp = "rows in COMPARE that are not in BASE.*columns in COMPARE that are not in BASE"
    )
    expect_snapshot(res)

})



test_that("can handle non-overlapping keys", {
    expect_warning(
        {
            x1 <- data.frame(ID = c("A", "C"), val = c(1, 2))
            x2 <- data.frame(ID = c("B", "C"), val = c(3, 2))
            res <- diffdf(x1, x2, "ID")
        },
        regexp = "rows in BASE that are not in COMPARE.*rows in COMPARE that are not in BASE"
    )
    expect_snapshot(res)
})


test_that("Can handle missing keys", {
    d1 <- tibble(
        v1 = 1,
        v2 = 1,
    )
    d2 <- tibble(
        v1 = 1,
        v2 = 1,
        v3 = 1,
        v4 = 1,
        v5 = 1
    )
    expect_error(
        diffdf(d1, d2, c("v1", "v2", "v3", "v4")),
        regexp = "not available in BASE.*   v3.*   v4"
    )
    expect_error(
        diffdf(d2, d1, c("v1", "v2", "v3", "v4")),
        regexp = "not available in COMPARE.*   v3.*   v4"
    )
})
