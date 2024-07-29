devtools::load_all()


suppressPackageStartupMessages({
    library(dplyr)
    library(stringi)
    library(lubridate)
    library(haven)
})


generate_test_data <- function(
    n = 10000,
    n_col = 4,
    n_num = n_col,
    n_int = n_col,
    n_chr = n_col,
    n_fct = n_col,
    n_date = n_col,
    n_dt = n_col
) {
    dat <- tibble(id = 1:n)

    for (i in seq_len(n_num)) {
        dat[sprintf("num_%s", i)] <- runif(n, -1000000, 1000000)
    }
    for (i in seq_len(n_int)) {
        dat[sprintf("int_%s", i)] <- as.integer(sample(seq(-9999, 9999), n, TRUE))
    }
    for (i in seq_len(n_chr)) {
        possible_chrs <- stringi::stri_rand_strings(1000, 15)
        dat[sprintf("chr_%s", i)] <- sample(possible_chrs, n, TRUE)
    }
    for (i in seq_len(n_fct)) {
        fct_levels <- c("A", "B", "C", "D", "E", "F", "G", "H")
        dat[sprintf("fct_%s", i)] <- factor(sample(fct_levels, size = n, replace = TRUE), levels = fct_levels)
    }
    for (i in seq_len(n_date)) {
        dat[sprintf("date_%s", i)] <- ymd("20200101") + days(round(runif(n, -1000, 1000)))
    }
    for (i in seq_len(n_dt)) {
        dat[sprintf("dt_%s", i)] <- ymd_hms("2020-01-01T12:00:01") + seconds(round(runif(n, -70000000, 70000000)))
    }
    return(dat)
}

dat1 <- generate_test_data(1000000, n_col = 10) |>
    sample_frac(1)

dat2 <- generate_test_data(1000000, n_col = 10) |>
    sample_frac(1)


results_new <- replicate(
    {
        x <- system.time({
            diffdf(dat1, dat2, "id", suppress_warnings = TRUE)
        })
        Sys.sleep(4)
        x
    },
    simplify = FALSE,
    n = 6
)


dat1 <- generate_test_data(1000, n_col = 1) |>
    sample_frac(1)

dat2 <- generate_test_data(1000, n_col = 1) |>
    sample_frac(1)

diffdf(dat1, dat2, "id")
