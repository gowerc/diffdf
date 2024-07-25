#' factor_to_character
#'
#' Takes a dataframe and converts any factor variables to character
#' @param dsin input dataframe
#' @param vars variables to consider for conversion. Default NULL will consider
#' every variable within the dataset
#' @keywords internal
factor_to_character <- function(dsin, vars = NULL) {
    if (is.null(vars)) vars <- names(dsin)

    for (var in vars) {
        if (is.factor(dsin[[var]])) {
            dsin[[var]] <- as.character(dsin[[var]])
        }
    }
    return(dsin)
}




#' has_unique_rows
#'
#' Check if a data sets rows are unique
#' @param DAT input data set (data frame)
#' @param KEYS Set of keys which should be unique
#' @keywords internal
has_unique_rows <- function(DAT, KEYS) {
    DUPS <- duplicated(subset(DAT, select = KEYS))
    NDUPS <- sum(DUPS)
    return(NDUPS == 0)
}

#' convert_to_issue
#'
#' converts the count value into the correct issue format
#' @param datin data inputted
#' @importFrom tibble rownames_to_column
#' @keywords internal
convert_to_issue <- function(datin) {
    datin_tibble <- tibble(
        `Variable` = names(datin),
        `No of Differences` = datin
    )

    datin_tibble_reduced <- datin_tibble[datin_tibble[["No of Differences"]] > 0, , drop = FALSE]
    return(datin_tibble_reduced)
}

#' Describe the datasets being compared
#'
#' This function is used to produce a basic summary table of the core
#' features of the two `data.frame`'s being compared.
#' @param base (`data.frame`)\cr base dataset to be described
#' @param comp (`data.frame`)\cr comparison dataset to be described
#' @param base_name (`character`)\cr name of the base dataset
#' @param comp_name (`character`)\cr name of the comparison dataset
#' @keywords internal
describe_dataframe <- function(base, comp, base_name, comp_name) {
    tibble(
        PROPERTY = list(
            "Name",
            "Class",
            "Rows(#)",
            "Columns(#)"
        ),
        BASE = c(
            base_name,
            paste(class(base), collapse = ", "),
            as.character(nrow(base)),
            as.character(ncol(base))
        ),
        COMP = c(
            comp_name,
            paste(class(comp), collapse = ", "),
            as.character(nrow(comp)),
            as.character(ncol(comp))
        )
    )
}
