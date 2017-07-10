
#' Compares dataframes
#' @description  Emulates proc compare from SAS
#' Compares 2 data frames and outputs any differences.
#' @param BASE input dataframe
#' @param COMP comparison dataframe
#' @param KEY list of variables (as strings) that defines a unique row in the dataframe
#' @import dplyr
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

    ### Convert keys to list for use in SE dplyr functions
    KEY.list <- as.list(KEY)

    #  Check the provided by groups define unique rows
    if ( !check_unique_rows(BASE , KEY.list) )
        stop( "BY variables in BASE do not result in unique observations")

    if ( !check_unique_rows(COMP , KEY.list) )
        stop( "BY variables in COMPARE do not result in unique observations")

    # Identify rows that exist in one dataset and not the other
    ROWCHECK1 <- BASE %>%
        anti_join( COMP , by = KEY) %>%
        select_(.dots = KEY.list)

    ROWCHECK2 <- COMP %>%
        anti_join( BASE , by = KEY) %>%
        select_(.dots = KEY.list)

    # Identify columns that exist in one dataset and not the other
    BASE.cols <- sapply ( names(BASE) , "%in%" ,   names(COMP)    )
    COMP.cols <- sapply ( names(COMP) , "%in%" ,   names(BASE)    )
    COLCHECK1 <- names(BASE.cols)[ !BASE.cols]
    COLCHECK2 <- names(COMP.cols)[ !COMP.cols]

    # Compare matching values
    COLS <- names(COMP.cols)[COMP.cols]
    VARS <- COLS[!COLS %in% KEY]


    # Filter down to matching rows and columms
    BASE2 <- BASE %>%
        inner_join( select_ ( COMP , .dots =KEY.list  ) , by= KEY) %>%
        select_( .dots = as.list(COLS)) %>%
        arrange_(.dots = KEY.list)

    COMP2 <- COMP %>%
        inner_join( select_ ( BASE , .dots =KEY.list  ) , by= KEY) %>%
        select_( .dots = as.list(COLS)) %>%
        arrange_(.dots = KEY.list)

    # Loop through each variable outputing where they don't match
    for ( VAR in VARS){
        cname <- paste0(VAR , c(".x" , ".y"))

        out <- BASE2 %>%
            select_ ( .dots =  c(KEY , VAR)) %>%
            inner_join(   COMP2 %>%  select_ ( .dots = c(KEY , VAR))   , by = KEY ) %>%
            rename_( .dots = setNames( as.list(cname),  c("BASE" , "COMPARE"))) %>%
            mutate( VARIABLE = VAR ) %>%
            select_ ( .dots = c(KEY , "VARIABLE" , "BASE" , "COMPARE") ) %>%
            filter (
                BASE != COMPARE |
                ( is.na(BASE) != is.na(COMPARE))  |
                (is.null(BASE) != is.null(COMPARE))
            )

        COMPARE[["Vars"]][[VAR]] <- out
    }

    COMPARE[["ExtRowsBase"]] <- ROWCHECK1
    COMPARE[["ExtRowsComp"]] <- ROWCHECK2
    COMPARE[["ExtColsBase"]] <- COLCHECK1
    COMPARE[["ExtColsComp"]] <- COLCHECK2

    SUMM <- sapply( COMPARE[["Vars"]] , nrow)
    COMPARE[["NumDiff"]] <- SUMM


    if(!SUPWARN){
        if( nrow(ROWCHECK1) )
            warning("There are Rows in BASE that are not in COMPARE" )

        if( nrow(ROWCHECK2) )
            warning("There are Rows in COMPARE that are not in BASE" )

        if ( length(COLCHECK1))
            warning("There are Columns in BASE that are not in COMPARE" )

        if ( length(COLCHECK2))
            warning("There are Columns in COMPARE that are not in BASE" )

        if( sum(SUMM))
            warning("Not all values compared equal")
    }

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
