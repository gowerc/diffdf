
#' Compares dataframes
#' @description  Emulates proc compare from SAS
#' Compares 2 data frames and outputs any differences.
#' @param BASE input dataframe
#' @param COMP comparison dataframe
#' @param KEYS list of variables (as strings) that defines a unique row in the dataframe
#' @param SUPWARN Do you want to suppress warnings
#' @import dplyr
#' @import purrr
#' @examples
#' ## compare( AAE , QC_AAE , c("USUBJID" , "AESEQ"))
#' @export
rcompare <- function (BASE , COMP , KEYS = NULL, SUPWARN = F){


    ### If no key is suplied match values based upon row number
    if (is.null(KEYS)){
        BASE <- BASE %>% mutate( ..ROWNUMBER.. = row_number())
        COMP <- COMP %>% mutate( ..ROWNUMBER.. = row_number())
        KEYS  <- "..ROWNUMBER.."
    }


    #  Check the provided by groups define unique rows
    if ( !check_unique_rows(BASE , KEYS) )
        stop( "BY variables in BASE do not result in unique observations")

    if ( !check_unique_rows(COMP , KEYS) )
        stop( "BY variables in COMPARE do not result in unique observations")


    ### Initatiate output object
    COMPARE <- list()
    class(COMPARE) <- "rcompare"

    COMPARE[["ExtRowsBase"]] <- identify_extra_rows(BASE, COMP , KEYS)
    COMPARE[["ExtRowsComp"]] <- identify_extra_rows(COMP, BASE , KEYS)

    COMPARE[["ExtColsBase"]] <- identify_extra_cols(BASE, COMP)
    COMPARE[["ExtColsComp"]] <- identify_extra_cols(COMP, BASE)
    
    COMPARE[["IllegalColsBase"]]    <- identify_ilegal_cols(BASE)
    COMPARE[["IllegalColsCompare"]] <- identify_ilegal_cols(COMP)
    
    #running list of columns to exclude from following checks
    
    exclude_cols <- c(names(COMPARE[["IllegalColsBase"]]), names(COMPARE[["IllegalColsCompare"]]))
    
    COMPARE[["VarModeDiffs"]] <- identify_mode_differences(BASE, COMP , KEYS, exclude_cols)
    
    
    if (nrow(COMPARE[["VarModeDiffs"]]))
    exclude_cols <- c(exclude_cols, COMPARE[["VarModeDiffs"]]$VARIABLE)
    
    COMPARE[["FactorlevelDiffs"]] <- identify_fact_level_differences(BASE, COMP , KEYS, exclude_cols)

    COMPARE[["VarDiffs"]] <- identify_differences(BASE, COMP , KEYS, exclude_cols)

    COMPARE[["LabelDiffs"]] <- identify_label_differences(BASE, COMP , KEYS, exclude_cols)
    
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
