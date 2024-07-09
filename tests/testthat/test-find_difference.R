
test_that("find_difference correctly doesn't flag identical objects", {
    expect_self <- function(x, name) {
        expect_true(!find_difference(x, x) %>% all(), label = name)
    }
    purrr::walk2(VALS, names(VALS), expect_self)
    expect_true(!find_difference(VALS$flt, VALS$flt_calc) %>% all())
})



test_that("find_difference correctly flags differences", {
    expect_equal(find_difference(VALS$num, VALS$num_na), rep(c(FALSE, TRUE), c(4, 1)))
    expect_equal(find_difference(VALS$flt, VALS$flt2), rep(c(FALSE), c(5)))
    expect_equal(find_difference(VALS$chr, VALS$chr_na), rep(c(FALSE, TRUE), c(3, 2)))
    expect_equal(find_difference(VALS$fct, VALS$fct_na), rep(c(FALSE, TRUE, FALSE), c(2, 1, 2)))
    expect_equal(find_difference(VALS$lgl, VALS$lgl_na), rep(c(FALSE, TRUE, FALSE), c(2, 1, 2)))
})
test_that("find_difference correctly uses tolerances/scale arguments", {
    expect_equal(find_difference(VALS$flt, VALS$flt2), rep(c(FALSE), c(5)))
    expect_equal(find_difference(VALS$flt, VALS$flt2, tolerance = 0.0000000000001), rep(c(TRUE), c(5)))
    expect_equal(find_difference(VALS$flt, VALS$flt3), rep(c(FALSE), c(5)))
    expect_equal(find_difference(VALS$flt, VALS$flt3, tolerance = 0.0000000000001), rep(c(TRUE, FALSE), c(1, 4)))
    expect_equal(find_difference(
        VALS$flt,
        VALS$flt2,
        tolerance = 1,
        scale = 1e-13
    ),
    rep(c(TRUE), c(5)))
    expect_equal(find_difference(VALS$flt, VALS$flt_calc), rep(c(FALSE), c(5)))
    expect_equal(
        find_difference(VALS$flt, VALS$flt_calc, tolerance = 1e-17),
        VALS$flt != VALS$flt_calc
    )
})

test_that("find_difference handles int64 correctly", {
    expect_equal(find_difference(
        bit64::as.integer64(3),
        bit64::as.integer64(4)
    ), c(TRUE))
    expect_equal(find_difference(
        bit64::as.integer64(3),
        bit64::as.integer64(5),
        tolerance = 2
    ), c(FALSE))
    expect_equal(find_difference(
        bit64::as.integer64(3),
        bit64::as.integer64(8),
        tolerance = 1,
        scale = 4
    ), c(TRUE))
    expect_equal(find_difference(
        bit64::as.integer64(3),
        bit64::as.integer64(8),
        tolerance = 1,
        scale = 8
    ), c(FALSE))
})




test_that("find_difference throws a warning if vectors are of a different length", {
    msg <- "Inputs are not of the same length"
    expect_warning(find_difference(c(1, 2, 3), c(1, 2, NULL)), regexp = msg)
    expect_warning(find_difference(c(1, 2, 3), c(1, 2)), regexp = msg)
})
