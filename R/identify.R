#' identify_extra_rows
#'
#' Identifies rows that are in a baseline dataset but not in a comparator dataset
#' @param DS1 Baseline dataset (data frame)
#' @param DS2 Comparator dataset (data frame)
#' @param KEYS List of variables that define a unique row within the datasets (strings)
#' @keywords internal
identify_extra_rows <- function(DS1, DS2, KEYS) {
    if (nrow(DS2) == 0 || nrow(DS1) == 0) {
        return(DS1[, KEYS, drop = FALSE])
    }
    DS2[["..FLAG.."]] <- "Y"
    dat <- merge(
        subset(DS1, select = KEYS),
        subset(DS2, select = c(KEYS, "..FLAG..")),
        by = KEYS, all.x = TRUE,
        sort = TRUE
    )
    dat <- dat[do.call("order", dat[KEYS]), ]

    dat[is.na(dat[["..FLAG.."]]), KEYS, drop = FALSE]
}



#' identify_extra_cols
#'
#' Identifies columns that are in a baseline dataset but not in a comparator dataset
#' @param DS1 Baseline dataset (data frame)
#' @param DS2 Comparator dataset (data frame)
#' @importFrom tibble tibble
#' @keywords internal
identify_extra_cols <- function(DS1, DS2) {
    match.cols <- sapply(names(DS1), "%in%", names(DS2))
    assertthat::assert_that(
        all(is.logical(match.cols)),
        msg = "Assumption of logical return type is not true"
    )
    tibble(
        COLUMNS = names(DS1)[!match.cols]
    )
}







#' identify_matching_cols
#'
#' Identifies columns with the same name in two data frames
#' @param DS1 Input dataset 1 (data frame)
#' @param DS2 Input dataset 2 (data frame)
#' @param EXCLUDE Columns to ignore
#' @keywords internal
identify_matching_cols <- function(DS1, DS2, EXCLUDE = "") {
    match_cols <- sapply(names(DS1), "%in%", names(DS2))
    exclude_cols <- sapply(names(DS1), "%in%", EXCLUDE)
    names(DS1)[match_cols & !exclude_cols]
}





#' identify_unsupported_cols
#'
#' Identifies any columns for which the package is not setup to handle
#' @param dsin input dataset
#' @keywords internal
identify_unsupported_cols <- function(dsin) {
    dat <- subset(
        identify_properties(dsin),
        select = c("VARIABLE", "MODE")
    )

    dat[!dat[["MODE"]] %in% c("numeric", "character", "logical"), , drop = FALSE]
}



#' identify_mode_differences
#'
#' Identifies any mode differences between two data frames
#' @param BASE Base dataset for comparison (data.frame)
#' @param COMP Comparator dataset to compare base against (data.frame)
#' @keywords internal
identify_mode_differences <- function(BASE, COMP) {
    matching_cols <- identify_matching_cols(BASE, COMP)

    dat <- merge(
        x = identify_properties(BASE),
        y = identify_properties(COMP),
        by = "VARIABLE",
        all = TRUE,
        suffixes = c(".BASE", ".COMP"),
        sort = TRUE
    )
    dat <- subset(dat, select = c("VARIABLE", "MODE.BASE", "MODE.COMP"))

    KEEP1 <- dat[["VARIABLE"]] %in% matching_cols
    KEEP2 <- dat[["MODE.BASE"]] != dat[["MODE.COMP"]]

    dat[KEEP1 & KEEP2, , drop = FALSE]
}



#' identify_class_differences
#'
#' Identifies any class differences between two data frames
#' @param BASE Base dataset for comparison (data.frame)
#' @param COMP Comparator dataset to compare base against (data.frame)
#' @keywords internal
identify_class_differences <- function(BASE, COMP) {
    matching_cols <- identify_matching_cols(BASE, COMP)

    dat <- merge(
        x = identify_properties(BASE),
        y = identify_properties(COMP),
        by = "VARIABLE",
        all = TRUE,
        sort = TRUE,
        suffixes = c(".BASE", ".COMP")
    )

    dat <- subset(dat, select = c("VARIABLE", "CLASS.BASE", "CLASS.COMP"))

    KEEP1 <- dat[["VARIABLE"]] %in% matching_cols
    KEEP2 <- !mapply(
        identical,
        dat[["CLASS.BASE"]],
        dat[["CLASS.COMP"]]
    )

    dat[KEEP1 & KEEP2, , drop = FALSE]
}



#' Identify differences in attributes
#'
#' Identifies any attribute differences between two data frames
#' @param BASE Base dataset for comparison (data.frame)
#' @param COMP Comparator dataset to compare base against (data.frame)
#' @param exclude_cols Columns to exclude from comparison
#' @importFrom tibble tibble
#' @keywords internal
identify_att_differences <- function(BASE, COMP, exclude_cols = "") {
    matching_cols <- identify_matching_cols(BASE, COMP, exclude_cols)

    PROPS <- merge(
        x = identify_properties(BASE),
        y = identify_properties(COMP),
        by = "VARIABLE",
        all = TRUE,
        sort = TRUE,
        suffixes = c(".BASE", ".COMP")
    )

    PROPS <- subset(PROPS, select = c("VARIABLE", "ATTRIBS.BASE", "ATTRIBS.COMP"))

    PROPS <- PROPS[PROPS[["VARIABLE"]] %in% matching_cols, , drop = FALSE]


    ### Setup dummy return value
    RETURN <- tibble(
        VARIABLE = character(),
        ATTR_NAME = character(),
        VALUES.BASE = list(),
        VALUES.COMP = list()
    )

    for (i in PROPS[["VARIABLE"]]) {
        PROPS_filt <- PROPS[PROPS[["VARIABLE"]] == i, , drop = FALSE]

        ### Get a vector of all available attributes across both variables
        ATTRIB_NAMES <- unique(c(
            names(PROPS_filt[["ATTRIBS.BASE"]][[1]]),
            names(PROPS_filt[["ATTRIBS.COMP"]][[1]])
        ))

        ### If variable has no attributes move onto the next variable
        if (is.null(ATTRIB_NAMES)) next()

        ### Loop over each attribute checking if they are identical and outputing
        ### anyones that arn't
        for (j in ATTRIB_NAMES) {
            ATTRIB_BASE <- PROPS_filt[["ATTRIBS.BASE"]][[1]][j]
            ATTRIB_COMP <- PROPS_filt[["ATTRIBS.COMP"]][[1]][j]

            if (!identical(ATTRIB_BASE, ATTRIB_COMP)) {
                ATT_DIFFS <- tibble(
                    VARIABLE = i,
                    ATTR_NAME = j,
                    VALUES.BASE = ifelse(is.null(ATTRIB_BASE), list(), ATTRIB_BASE),
                    VALUES.COMP = ifelse(is.null(ATTRIB_COMP), list(), ATTRIB_COMP)
                )

                RETURN <- rbind(RETURN, ATT_DIFFS)
            }
        }
    }
    return(RETURN)
}





#' identify_differences
#'
#' Compares each column within 2 datasets to identify any values which they
#' mismatch on.
#' @param BASE Base dataset for comparison (data.frame)
#' @param COMP Comparator dataset to compare base against (data.frame)
#' @param KEYS List of variables that define a unique row within the datasets (strings)
#' @param exclude_cols Columns to exclude from comparison
#' @param tolerance Level of tolerance for numeric differences between two variables
#' @param scale Scale that tolerance should be set on. If NULL assume absolute
#' @keywords internal
identify_differences <- function(
    BASE,
    COMP,
    KEYS,
    exclude_cols,
    tolerance = sqrt(.Machine$double.eps),
    scale = NULL
) {

    matching_cols <- identify_matching_cols(BASE, COMP, c(KEYS, exclude_cols))

    if (length(matching_cols) == 0) {
        return(tibble())
    }

    DAT <- merge(
        x = BASE,
        y = COMP,
        by = KEYS,
        suffix = c(".x", ".y"),
        sort = TRUE
    )
    if (nrow(DAT) == 0) {
        return(tibble())
    }
    DAT <- DAT[do.call("order", DAT[KEYS]), ]

    matching_list <- mapply(
        is_variable_different,
        matching_cols,
        MoreArgs = list(
            keynames = KEYS,
            datain = DAT,
            tolerance = tolerance,
            scale = scale
        ),
        SIMPLIFY = FALSE
    )

    matching_list
}









#' identify_properties
#'
#' Returns a dataframe of metadata for a given dataset.
#' Returned values include variable names , class , mode , type & attributes
#' @param dsin input dataframe that you want to get the metadata from
#' @importFrom tibble tibble
#' @keywords internal
identify_properties <- function(dsin) {
    ### If missing or null return empty dataset
    if (is.null(dsin)) {
        x <- tibble(
            VARIABLE = character(),
            CLASS = list(),
            MODE = character(),
            TYPE = character(),
            ATTRIBS = list()
        )
        return(x)
    }

    tibble(
        VARIABLE = names(dsin),
        CLASS = lapply(dsin, class),
        MODE = sapply(dsin, mode),
        TYPE = sapply(dsin, typeof),
        ATTRIBS = lapply(dsin, attributes)
    )
}


#' Find column ordering differences
#'
#' Compares two datasets and outputs a table listing any differences in the column
#' orders between the two datasets. Columns that are not contained within both
#' are ignored however column ordering is derived prior to removing these columns.
#'
#' @param BASE (`data.frame`)\cr Base dataset for comparison
#' @param COMP (`data.frame`)\cr Comparator dataset to compare base against
#' @keywords internal
identify_column_order_differences <- function(BASE, COMP) {
    base_cols <- tibble(
        COLUMN = names(BASE),
        "BASE-INDEX" = seq_along(names(BASE))
    )
    comp_cols <- tibble(
        COLUMN = names(COMP),
        "COMPARE-INDEX" = seq_along(names(COMP))
    )
    col_index <- merge(
        base_cols,
        comp_cols,
        by = c("COLUMN"),
        all = TRUE,
        sort = FALSE
    )
    keep_rows <- col_index[["BASE-INDEX"]] != col_index[["COMPARE-INDEX"]]
    keep_rows[is.na(keep_rows)] <- FALSE
    col_index[keep_rows, , drop = FALSE]
}
