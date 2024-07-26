#' diffdf
#' @description
#' Compares 2 dataframes and outputs any differences.
#' @param base input dataframe
#' @param compare comparison dataframe
#' @param keys vector of variables (as strings) that defines a unique row in
#' the base and compare dataframes
#' @param strict_numeric Flag for strict numeric to numeric comparisons
#' (default = TRUE). If False diffdf will cast integer to double where
#' required for comparisons. Note that variables specified in the keys
#' will never be casted.
#' @param strict_factor Flag for strict factor to character comparisons
#' (default = TRUE). If False diffdf will cast factors to characters where
#' required for comparisons. Note that variables specified in the keys will
#' never be casted.
#' @param suppress_warnings Do you want to suppress warnings? (logical)
#' @param file Location and name of a text file to output the results to.
#' Setting to NULL will cause no file to be produced.
#' @param tolerance Set tolerance for numeric comparisons. Note that
#' comparisons fail if (x-y)/scale > tolerance.
#' @param scale Set scale for numeric comparisons. Note that comparisons fail
#' if (x-y)/scale > tolerance. Setting as NULL is a slightly more efficient
#' version of scale = 1.
#' @param check_column_order Should the column ordering be checked? (logical)
#' @param check_df_class Do you want to check for differences in the class
#' between `base` and `compare`? (logical)
#' @examples
#' x <- subset(iris, -Species)
#' x[1, 2] <- 5
#' COMPARE <- diffdf(iris, x)
#' print(COMPARE)
#'
#' #### Sample data frames
#'
#' DF1 <- data.frame(
#'     id = c(1, 2, 3, 4, 5, 6),
#'     v1 = letters[1:6],
#'     v2 = c(NA, NA, 1, 2, 3, NA)
#' )
#'
#' DF2 <- data.frame(
#'     id = c(1, 2, 3, 4, 5, 7),
#'     v1 = letters[1:6],
#'     v2 = c(NA, NA, 1, 2, NA, NA),
#'     v3 = c(NA, NA, 1, 2, NA, 4)
#' )
#'
#' diffdf(DF1, DF1, keys = "id")
#'
#' # We can control matching with scale/location for example:
#'
#' DF1 <- data.frame(
#'     id = c(1, 2, 3, 4, 5, 6),
#'     v1 = letters[1:6],
#'     v2 = c(1, 2, 3, 4, 5, 6)
#' )
#' DF2 <- data.frame(
#'     id = c(1, 2, 3, 4, 5, 6),
#'     v1 = letters[1:6],
#'     v2 = c(1.1, 2, 3, 4, 5, 6)
#' )
#'
#' diffdf(DF1, DF2, keys = "id")
#' diffdf(DF1, DF2, keys = "id", tolerance = 0.2)
#' diffdf(DF1, DF2, keys = "id", scale = 10, tolerance = 0.2)
#'
#' # We can use strict_factor to compare factors with characters for example:
#'
#' DF1 <- data.frame(
#'     id = c(1, 2, 3, 4, 5, 6),
#'     v1 = letters[1:6],
#'     v2 = c(NA, NA, 1, 2, 3, NA),
#'     stringsAsFactors = FALSE
#' )
#'
#' DF2 <- data.frame(
#'     id = c(1, 2, 3, 4, 5, 6),
#'     v1 = letters[1:6],
#'     v2 = c(NA, NA, 1, 2, 3, NA)
#' )
#'
#' diffdf(DF1, DF2, keys = "id", strict_factor = TRUE)
#' diffdf(DF1, DF2, keys = "id", strict_factor = FALSE)
#'
#' @export
diffdf <- function(
    base,
    compare,
    keys = NULL,
    suppress_warnings = FALSE,
    strict_numeric = TRUE,
    strict_factor = TRUE,
    file = NULL,
    tolerance = sqrt(.Machine$double.eps),
    scale = NULL,
    check_column_order = FALSE,
    check_df_class = FALSE
) {

    assertthat::assert_that(
        assertthat::is.flag(check_df_class),
        !is.na(check_df_class),
        msg = "`check_df_class` must be a length 1 logical"
    )

    BASE <- base
    COMP <- compare
    KEYS <- keys
    SUPWARN <- suppress_warnings

    ### Initatiate output object
    COMPARE <- list()
    class(COMPARE) <- c("diffdf", "list")


    BASE_NAME <- deparse(substitute(base))
    COMP_NAME <- deparse(substitute(compare))
    COMPARE[["DataSummary"]] <- construct_issue(
        value = describe_dataframe(BASE, COMP, BASE_NAME, COMP_NAME),
        message = "Summary of BASE and COMPARE"
    )


    is_derived <- FALSE

    ### If no key is suplied match values based upon row number
    if (is.null(KEYS)) {
        is_derived <- TRUE
        keyname <- generate_keyname(BASE, COMP)
        BASE[[keyname]] <- seq_len(nrow(BASE))
        COMP[[keyname]] <- seq_len(nrow(COMP))
        KEYS <- keyname
    }
    attr(COMPARE, "keys") <- list(value = KEYS, is_derived = is_derived)

    assertthat::assert_that(
        is.numeric(tolerance),
        is.numeric(scale) || is.null(scale)
    )

    missing_keys_base <- KEYS[!KEYS %in% names(BASE)]
    assertthat::assert_that(
        length(missing_keys_base) == 0,
        msg = sprintf(
            "The following KEYS are not available in BASE:\n   %s",
            paste(missing_keys_base, collapse = "\n   ")
        )
    )

    missing_keys_comp <- KEYS[!KEYS %in% names(COMP)]
    assertthat::assert_that(
        length(missing_keys_comp) == 0,
        msg = sprintf(
            "The following KEYS are not available in COMPARE:\n   %s",
            paste(missing_keys_comp, collapse = "\n   ")
        )
    )

    assertthat::assert_that(
        has_unique_rows(BASE, KEYS),
        msg = "BY variables in BASE do not result in unique observations"
    )

    assertthat::assert_that(
        has_unique_rows(COMP, KEYS),
        msg = "BY variables in COMPARE do not result in unique observations"
    )



    #### Check essential variable properties (class & mode)

    COMPARE[["UnsupportedColsBase"]] <- construct_issue(
        value = identify_unsupported_cols(BASE),
        message = "There are columns in BASE with unsupported modes !!"
    )


    COMPARE[["UnsupportedColsComp"]] <- construct_issue(
        value = identify_unsupported_cols(COMP),
        message = "There are columns in COMPARE with unsupported modes !!"
    )


    # cast variables if strict is off
    if (!strict_factor || !strict_numeric) {
        casted_df <- cast_variables(
            BASE = BASE,
            COMPARE = COMP,
            ignore_vars = KEYS,
            cast_integers = !strict_numeric,
            cast_factors = !strict_factor
        )

        BASE <- casted_df$BASE
        COMP <- casted_df$COMP
    }


    COMPARE[["VarModeDiffs"]] <- construct_issue(
        value = identify_mode_differences(BASE, COMP),
        message = "There are columns in BASE and COMPARE with different modes !!"
    )


    COMPARE[["VarClassDiffs"]] <- construct_issue(
        value = identify_class_differences(BASE, COMP),
        message = "There are columns in BASE and COMPARE with different classes !!"
    )




    ##### Check Validity of Keys

    BASE_keys <- names(BASE)[names(BASE) %in% KEYS]
    COMP_keys <- names(COMP)[names(COMP) %in% KEYS]

    assertthat::assert_that(
        length(BASE_keys) == length(KEYS),
        msg = "BASE is missing variables specified in KEYS"
    )

    assertthat::assert_that(
        length(COMP_keys) == length(KEYS),
        msg = "COMP is missing variables specified in KEYS"
    )


    assert_valid_keys(
        COMPARE, KEYS, "UnsupportedColsBase",
        "The following KEYS in BASE have an unsupported mode (see `?mode()`)"
    )
    assert_valid_keys(
        COMPARE, KEYS, "UnsupportedColsComp",
        "The following KEYS in COMPARE have an unsupported mode (see `?mode()`)"
    )
    assert_valid_keys(
        COMPARE, KEYS, "VarModeDiffs",
        "The following KEYS have different modes between BASE and COMPARE"
    )
    assert_valid_keys(
        COMPARE, KEYS, "VarClassDiffs",
        "The following KEYS have different classes between BASE and COMPARE"
    )


    exclude_cols <- c(
        COMPARE[["UnsupportedColsBase"]]$VARIABLE,
        COMPARE[["UnsupportedColsComp"]]$VARIABLE,
        COMPARE[["VarClassDiffs"]]$VARIABLE,
        COMPARE[["VarModeDiffs"]]$VARIABLE
    )

    if (check_column_order) {
        if (attr(COMPARE, "keys")$is_derived) {
            keep_vars_base <- !(names(BASE) %in% attr(COMPARE, "keys")$value)
            keep_vars_comp <- !(names(COMP) %in% attr(COMPARE, "keys")$value)
        } else {
            keep_vars_base <- TRUE
            keep_vars_comp <- TRUE
        }
        COMPARE[["ColumnOrder"]] <- construct_issue(
            value = identify_column_order_differences(
                BASE[, keep_vars_base, drop = FALSE],
                COMP[, keep_vars_comp, drop = FALSE]
            ),
            message = "There are differences in the column ordering between BASE and COMPARE !!"
        )
    }


    ##### Check Attributes
    COMPARE[["AttribDiffs"]] <- construct_issue(
        value = identify_att_differences(BASE, COMP, exclude_cols),
        message = "There are columns in BASE and COMPARE with differing attributes !!"
    )


    ##### Check data

    BASE <- factor_to_character(BASE, KEYS)
    COMP <- factor_to_character(COMP, KEYS)


    COMPARE[["ExtRowsBase"]] <- construct_issue(
        value = identify_extra_rows(BASE, COMP, KEYS),
        message = "There are rows in BASE that are not in COMPARE !!"
    )


    COMPARE[["ExtRowsComp"]] <- construct_issue(
        value = identify_extra_rows(COMP, BASE, KEYS),
        message = "There are rows in COMPARE that are not in BASE !!"
    )


    COMPARE[["ExtColsBase"]] <- construct_issue(
        value = identify_extra_cols(BASE, COMP),
        message = "There are columns in BASE that are not in COMPARE !!"
    )


    COMPARE[["ExtColsComp"]] <- construct_issue(
        value = identify_extra_cols(COMP, BASE),
        message = "There are columns in COMPARE that are not in BASE !!"
    )


    VALUE_DIFFERENCES <- identify_differences(
        BASE, COMP, KEYS, exclude_cols,
        tolerance = tolerance,
        scale = scale
    )



    ## Summarise the number of mismatching rows per variable

    if (length(VALUE_DIFFERENCES)) {
        NDIFF <- sapply(VALUE_DIFFERENCES, nrow)
        COMPARE[["NumDiff"]] <- construct_issue(
            value = convert_to_issue(NDIFF),
            message = "Not all Values Compared Equal"
        )
    }


    for (i in names(VALUE_DIFFERENCES)) {
        COMPARE[[paste0("VarDiff_", i)]] <- construct_issue(
            value = VALUE_DIFFERENCES[[i]],
            message = NULL
        )
    }


    # suppress warning message of data summary if user didn't request to check it
    # we leave the issue in the main compare object though for printing purposes
    COMPARE_WARNINGS <- COMPARE
    attr(COMPARE_WARNINGS[["DataSummary"]], "message") <- c(
        "There are differences between the class of BASE and COMPARE"
    )
    if (!check_df_class || identical(class(base), class(compare))) {
        COMPARE_WARNINGS["DataSummary"] <- NULL
    }

    # Get all issue messages, remove blank message, and collapse into single string
    ISSUE_MSGS <- sapply(COMPARE_WARNINGS, function(x) get_issue_message(x))
    ISSUE_MSGS <- Filter(function(x) !is.null(x), ISSUE_MSGS)
    ISSUE_MSGS <- Filter(function(x) x != "", ISSUE_MSGS)

    if (length(ISSUE_MSGS) != 0) {
        if (!SUPWARN) {
            ISSUE_MSGS <- paste(ISSUE_MSGS, collapse = "\n")
            warning(c("\n", ISSUE_MSGS))
        }
    }

    # If the classes are the same and it is the only entry in the compare
    # object then remove it in order to trigger "no issues found"
    if (identical(class(base), class(compare)) && length(COMPARE) == 1) {
        COMPARE["DataSummary"] <- NULL
    }

    # If the summary is the only item and the user didn't want to check classes
    # then remove the object to trigger the "no issues found"
    if (!check_df_class && length(COMPARE) == 1) {
        COMPARE["DataSummary"] <- NULL
    }


    if (!is.null(file)) {
        x <- print(COMPARE, as_string = TRUE)

        tryCatch(
            {
                sink(file)
                cat(x, sep = "\n")
                sink()
            },
            warning = function(w) {
                sink()
                warning(w)
            },
            error = function(e) {
                sink()
                stop(e)
            }
        )
        return(invisible(COMPARE))
    }

    return(COMPARE)
}




#' diffdf_has_issues
#'
#' Utility function which returns TRUE if an diffdf
#' object has issues or FALSE if an diffdf object does not have issues
#' @param x diffdf object
#' @examples
#'
#' # Example with no issues
#' x <- diffdf(iris, iris)
#' diffdf_has_issues(x)
#'
#' # Example with issues
#' iris2 <- iris
#' iris2[2, 2] <- NA
#' x <- diffdf(iris, iris2, suppress_warnings = TRUE)
#' diffdf_has_issues(x)
#' @export
diffdf_has_issues <- function(x) {
    if (class(x)[[1]] != "diffdf") stop("x is not an diffdf object")
    return(length(x) != 0)
}


#' Assert that keys are valid
#'
#' Utility function to check that user provided "keys" aren't listed as a problem
#' variable of the current list of issues.
#' @param COMPARE (`list`)\cr A named list of which each element is a `data.frame` with the
#' column `VARIABLE`
#' @param KEYS (`character`)\cr name of key variables to check to make sure they don't contain
#' any issues
#' @param component (`character`)\cr name of the component within `COMPARE` to check against
#' @param msg (`character`)\cr error message to print if any of `KEYS` are found within
#' `COMPARE[component]$VARIABLE`
#' @keywords internal
assert_valid_keys <- function(COMPARE, KEYS, component, msg) {
    keys_reduced <- KEYS[KEYS %in% COMPARE[[component]]$VARIABLE]
    assertthat::assert_that(
        length(keys_reduced) == 0,
        msg = sprintf(
            "%s:\n%s",
            msg,
            paste0("`", paste0(keys_reduced, collapse = "`, `"), "`")
        )
    )
}
