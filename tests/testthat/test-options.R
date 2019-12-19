context("Check option checking works correctly")

default <- diffdf:::options_default

test_that("Setting options works correctly and can be reset",{
    expect_equal(diffdf_options(),
                 default)
    
    new <- default
    new$warnings <- TRUE
    new$file <- "apath"
    expect_equal(diffdf_options(warnings = TRUE, file = "apath"),
                 new)
    new$strict_numeric <- FALSE
    new$strict_factor <- FALSE
    new$tolerance <- 100
    new$scale <- 1
    expect_equal(
        diffdf_options(
            strict_numeric = FALSE,
            strict_factor = FALSE,
            tolerance = 100,
            scale = 1
        ),
        new
    )
    expect_equal(diffdf_options(), new)
    diffdf_options_reset()
    
    expect_equal(diffdf_options(),
                 default)
              
})

test_that("Giving misnamed options provides a warning",{
    
    expect_warning(diffdf_options(badarg = TRUE))
    
})

test_that("Giving bad options provides an error",{
    
    expect_error(diffdf_options(warnings = 5))
    expect_error(diffdf_options(warnings = c(TRUE, TRUE)))
    expect_error(diffdf_options(strict_numeric = c(TRUE, TRUE)))
    expect_error(diffdf_options(strict_numeric = 7))
    expect_error(diffdf_options(strict_factor = c(TRUE, TRUE)))
    expect_error(diffdf_options(strict_factor = 7))
    expect_error(diffdf_options(file = 7))
    expect_error(diffdf_options(file = c("a", "b")))
    expect_error(diffdf_options(file = list()))
    expect_error(diffdf_options(tolerance = "test"))
    expect_error(diffdf_options(tolerance = c(1,2)))
    expect_error(diffdf_options(scale = "test"))
    expect_error(diffdf_options(scale = c(1,2)))
    
})