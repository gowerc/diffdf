


test_that("Keys with different modes have variable name printed", {
    dat1 <- tibble(
        key1 = c("1", "2", "3"),
        key2 = c(1, 2, 3),
        key3 = c("1", "2", "3"),
        val = c(1, 2, 3)
    )
    dat2 <- tibble(
        key1 = c(1, 2, 3),
        key2 = c(1, 2, 3),
        key3 = c(1, 2, 3),
        val = c(1, 2, 3)
    )
    expect_error(
        diffdf(dat1, dat2, c("key1", "key2", "key3")),
        regex = "modes between BASE and COMPARE.*`key1`, `key3`"
    )
    expect_error(
        diffdf(dat2, dat1, c("key1", "key2", "key3")),
        regex = "modes between BASE and COMPARE.*`key1`, `key3`"
    )
})


test_that("Keys with different classes have variable name printed", {
    dat1 <- tibble(
        key1 = structure(1, class = "A"),
        key2 = structure(1, class = "A"),
        key3 = 1,
        value = 2
    )
    dat2 <- tibble(
        key1 = structure(1, class = "A"),
        key2 = structure(1, class = "B"),
        key3 = 1,
        value = 2
    )
    expect_error(
        diffdf(dat1, dat2, c("key1", "key2", "key3")),
        regex = "different classes.*`key2`"
    )
})


test_that("Keys with an unsupported mode have variable name printed", {
    dat1 <- tibble(
        key1 = list(1),
        key2 = structure(1, class = "A"),
        key3 = list(1),
        value = 2
    )
    dat2 <- tibble(
        key1 = structure(1, class = "A"),
        key2 = structure(1, class = "B"),
        key3 = 1,
        value = 2
    )
    expect_error(
        diffdf(dat1, dat2, c("key1", "key2", "key3")),
        regex = "KEYS in BASE.*`key1`, `key3`"
    )
    expect_error(
        diffdf(dat2, dat1, c("key1", "key2", "key3")),
        regex = "KEYS in COMPARE.*`key1`, `key3`"
    )
})
