

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


test_that("Format Char works on standard data types", {

    expect_equal(
        as_fmt_char(c(1.12345678, 2, 3, 4)),
        c("1.123457", "2.000000", "3.000000", "4.000000")
    )

    expect_equal(
        as_fmt_char(c("A", "awdaiowjdidddddoawj awijdwwwwwoiawjd", "B", "C")),
        c("A", "\"awdaiowjdidddddoawj awijdwwwww...\"", "B", "C")
    )

    expect_equal(
        as_fmt_char(factor(c("A", "B", "C", "adiawi iawjdoi  aiwjdioawj iawjdoa"))),
        c("A", "B", "C", "\"adiawi iawjdoi  aiwjdioawj iaw...\"")
    )

    expect_equal(
        as_fmt_char(c(TRUE, NA, FALSE)),
        c("TRUE", "<NA>", "FALSE")
    )

    expect_equal(
        as_fmt_char(lubridate::ymd("2020-01-01", "1888-01-29 UTC")),
        c("2020-01-01", "1888-01-29")
    )

    expect_equal(
        as_fmt_char(lubridate::ymd_hms(
            c(
                "2023-01-01 14:12:12",
                "2023-07-05 15:45:30"
            ),
            tz = "Europe/London"
        )),
        c("2023-01-01 14:12:12 GMT", "2023-07-05 15:45:30 BST")
    )

    x <- c(1, 2, 3, 4)
    class(x) <- "some random class"
    expect_equal(
        as_fmt_char(x),
        c("1", "2", "3", "4")
    )

    # Test that as_fmt_char doesn't enter inf loop if
    # as.character does't return a character
    x <- 1
    class(x) <- c("myclass", "myclass2")
    testthat::with_mocked_bindings(
        expect_error(
            as_fmt_char(x),
            regexp = "`'myclass', 'myclass2'`"
        ),
        as_character = function(x) x,
    )
})


test_that("ascii_table can handle all standard datatypes", {
    expect_snapshot(as_ascii_table(TDAT) |> cat())
})


test_that("datetimes compare as expected", {

    # Same character values but different underlying numerics
    d1 <- tibble(
        id = c(1, 2),
        dt1 = lubridate::ymd_hms(
            "2024-01-10 01-02-03",
            "2024-01-24 14-12-49",
            tz = "EST"
        )
    )
    d2 <- tibble(
        id = c(1, 2),
        dt1 = lubridate::ymd_hms(
            "2024-01-10 01-02-03",
            "2024-01-24 14-12-49",
            tz = "CET"
        )
    )
    expect_warning(
        res <- diffdf(d1, d2, "id"),
        regexp = "differing attributes.*Not all Values Compared Equal"
    )
    expect_snapshot(
        print(res)
    )



    # Same underlying numerics but different character values
    d1 <- tibble(
        id = c(1, 2),
        dt1 = lubridate::ymd_hms(
            "2024-01-10 01-02-03",
            "2024-01-24 14-12-49",
            tz = "EST"
        )
    )
    d2 <- d1
    d2$dt1 <- lubridate::with_tz(d2$dt1, tzone = "CET")
    expect_warning(
        res <- diffdf(d1, d2, "id"),
        regexp = "differing attributes[ !]*$"
    )
    expect_snapshot(
        print(res)
    )
})
