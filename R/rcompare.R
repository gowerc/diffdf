
#' Compares dataframes
#' @description  Emulates proc compare from SAS
#' Compares 2 data frames and outputs any differences.
#' @param BASE input dataframe
#' @param COMP comparison dataframe
#' @param KEY list of variables (as strings) that defines a unique row in the dataframe
#' @param SUPWARN Do you want to suppress warnings
#' @import dplyr
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom stats setNames
#' @examples
#' ## compare( AAE , QC_AAE , c("USUBJID" , "AESEQ"))
#' @export
rcompare <- function (BASE , COMP , KEY = NULL, SUPWARN = F){


    ### If no key is suplied match values based upon row number
    if (is.null(KEY)){
        BASE <- BASE %>% mutate( ..ROWNUMBER.. = row_number())
        COMP <- COMP %>% mutate( ..ROWNUMBER.. = row_number())
        KEY <- "..ROWNUMBER.."
    }

    ### Initatiate output object
    COMPARE <- list()
    class(COMPARE) <- "rcompare"

    #  Check the provided by groups define unique rows
    if ( !check_unique_rows(BASE , KEY) )
        stop( "BY variables in BASE do not result in unique observations")

    if ( !check_unique_rows(COMP , KEY) )
        stop( "BY variables in COMPARE do not result in unique observations")


    COMPARE[["ExtRowsBase"]] <- identify_extra_rows(BASE, COMP , KEY)
    COMPARE[["ExtRowsComp"]] <- identify_extra_rows(COMP, BASE , KEY)


    COMPARE[["ExtColsBase"]] <- identify_extra_cols(BASE, COMP)
    COMPARE[["ExtColsComp"]] <- identify_extra_cols(COMP, BASE)


    COMPARE[["VarDiffs"]] <- map(
        identify_matching_cols(BASE , COMP, KEY ) ,
        identify_value_diffs,
        DS1 = BASE ,
        DS2 = COMP ,
        KEY = KEY
    ) %>%
        set_names( identify_matching_cols(BASE , COMP, KEY ) )


    ### Summarise the number of mismatching rows per variable
    COMPARE[["NumDiff"]] <- sapply( COMPARE[["VarDiffs"]] , nrow)


    COMPARE[["Issues"]] <- check_for_issues( COMPARE, SUPWARN)


    return(COMPARE)
}



#### Example of use

# DF1 <- data.frame(
#     id = c(1,2,3,4,5,6),
#     v1 = letters[1:6],
#     v2 = c(NA , NA , 1 , 2 , 3 , NA)
# )
#
# DF2 <- data.frame(
#     id = c(1,2,3,4,5,7),
#     v1 = letters[1:6],
#     v2 = c(NA , NA , 1 , 2 , NA , NA),
#     v3 = c(NA , NA , 1 , 2 , NA , 4)
# )
#
# rcompare(DF1 , DF2 , KEY = "id")
