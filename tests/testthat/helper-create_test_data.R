library(testthat)
library(diffdf)
library(purrr)
library(tibble)

suppressWarnings(RNGversion("3.5.0"))
set.seed(20202)

LENGTH <- 20

TDAT <- dplyr::tibble(
    ID          = 1:LENGTH,
    GROUP1      = rep(c(1, 2), each = LENGTH / 2),
    GROUP2      = rep(c(1:(LENGTH / 2)), 2),
    INTEGER     = rpois(LENGTH, 40),
    BINARY      = sample(c("M", "F"), LENGTH, replace = TRUE),
    DATE        = lubridate::ymd("2000-01-01") + rnorm(LENGTH, 0, 7000),
    DATETIME    = lubridate::ymd_hms("2000-01-01 00:00:00") + rnorm(LENGTH, 0, 200000000),
    CONTINUOUS  = rnorm(LENGTH, 30, 12),
    CATEGORICAL = factor(sample(c("A", "B", "C"), LENGTH, replace = TRUE)),
    LOGICAL     = sample(c(TRUE, FALSE), LENGTH, replace = TRUE),
    CHARACTER   = stringi::stri_rand_strings(LENGTH, rpois(LENGTH, 13), pattern = "[ A-Za-z0-9]")
)


VALS <- list(
    int = 1:5,
    int_na = c(1:5, NA),
    num = c(1, 2, 3, 4, 5),
    num_na = c(1, 2, 3, 4, NA),
    flt = c(0, 0.1, 0.2, 0.3, 0.4),
    flt2 = c(0, 0.1, 0.2, 0.3, 0.4) - 0.000000000001,
    flt3 = c(0 + 1e-12, 0.1, 0.2, 0.3, 0.4),
    flt_calc = c(0.08, 0.18, 0.28, 0.38, 0.48) - 0.08,
    chr = c("antelope", "bear", "cake", "gpro/", "^admw"),
    chr_na = c("antelope", "bear", "cake", NA, "@awd"),
    chr_one = "duck",
    fct = factor(c("apple", "ball", "2", TRUE, "pears")),
    fct_na = factor(c("apple", "ball", NA, TRUE, "pears")),
    lgl = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    lgl_na = c(TRUE, FALSE, NA, TRUE, FALSE),
    null = NULL,
    na = NA
)



### Setup test data
TDAT2 <- TDAT

## Unequal values
TDAT2$CONTINUOUS[c(1, 5, 7)] <- c(1, 2, 3)

## Different attributes
attr(TDAT2$BINARY, "something") <- iris

## Different levels
levels(TDAT2$CATEGORICAL) <- c("A", "B", "D")

## Different class
class(TDAT2$DATE) <- c("A_DATE", "b_date", "cDate")

## Different mode
TDAT2$INTEGER[c(1, 5, 7)] <- c("1", "2", "3")

## Large number of differences
TDAT2$GROUP2 <- rpois(20, 50)

## Different labels
attr(TDAT2$DATETIME, "label") <- "This is the label for my amazing variable"




### Create large list of comparisons
list_of_comparisons <- list(
    "Identical" = list(
        TDAT[, "ID", drop = FALSE],
        TDAT2[, "ID", drop = FALSE]
    ),
    "Identical 2" = list(
        TDAT2[, "ID", drop = FALSE],
        TDAT[, "ID", drop = FALSE]
    ),
    "Different Values" = list(
        TDAT[, c("ID", "CONTINUOUS")],
        TDAT2[, c("ID", "CONTINUOUS")]
    ),
    "Different Values 2" = list(
        TDAT2[c("ID", "CONTINUOUS")],
        TDAT[c("ID", "CONTINUOUS")]
    ),
    "Different attributes" = list(
        TDAT[, c("ID", "BINARY")],
        TDAT2[, c("ID", "BINARY")]
    ),
    "Different attributes 2" = list(
        TDAT2[, c("ID", "BINARY")],
        TDAT[, c("ID", "BINARY")]
    ),
    "Different Levels" = list(
        TDAT[, c("ID", "CATEGORICAL")],
        TDAT2[, c("ID", "CATEGORICAL")]
    ),
    "Different Levels 2" = list(
        TDAT2[, c("ID", "CATEGORICAL")],
        TDAT[, c("ID", "CATEGORICAL")]
    ),
    "Different Class" = list(
        TDAT[, c("ID", "DATE")],
        TDAT2[, c("ID", "DATE")]
    ),
    "Different Class 2" = list(
        TDAT2[, c("ID", "DATE")],
        TDAT[, c("ID", "DATE")]
    ),
    "Different Modes" = list(
        TDAT[, c("ID", "INTEGER")],
        TDAT2[, c("ID", "INTEGER")]
    ),
    "Different Modes 2" = list(
        TDAT2[, c("ID", "INTEGER")],
        TDAT[, c("ID", "INTEGER")]
    ),
    "Missing Columns" = list(
        TDAT[, c("ID", "INTEGER", "BINARY")],
        TDAT2[, c("ID", "INTEGER")]
    ),
    "Missing Columns 2" = list(
        TDAT2[, c("ID", "INTEGER", "BINARY")],
        TDAT[, c("ID", "INTEGER")]
    ),
    "Missing Rows" = list(
        TDAT[, c("ID", "GROUP1")],
        TDAT2[seq_len(nrow(TDAT2)) <= 10, c("ID", "GROUP1")]
    ),
    "Missing Rows 2" = list(
        TDAT2[, c("ID", "GROUP1")],
        TDAT[seq_len(nrow(TDAT2)) <= 10, c("ID", "GROUP1")]
    ),
    "everything" = list(
        TDAT,
        TDAT2
    ),
    "everything 2" = list(
        TDAT2,
        TDAT
    ),
    "Missing Vs NA" = list(
        {
            M_TDAT <- TDAT
            M_TDAT$CHARACTER[1] <- NA
            M_TDAT
        },
        {
            M_TDAT2 <- TDAT
            M_TDAT2$CHARACTER[1] <- ""
            M_TDAT2
        }
    )
)
