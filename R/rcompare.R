
#' rcompare
#' @description  
#' Compares 2 data frames and outputs any differences.
#' Emulates proc compare from SAS
#' @param base input dataframe
#' @param compare comparison dataframe
#' @param keys vector of variables (as strings) that defines a unique row in the base and compare dataframes
#' @param suppress_warnings Do you want to suppress warnings? (logical)
#' @param outfile Location and name of a file to output the results to. Setting to NULL will cause no file to be produced.
#' @param tolerance Level of tolerance for numeric differences between two variables
#' @param scale Scale that tolerance should be set on. If NULL assume absolute
#' @importFrom purrr map_dbl
#' @importFrom purrr map_chr
#' @importFrom purrr "%>%"
#' @examples
#' x <- subset( iris,  -Species)
#' x[1,2] <- 5
#' COMPARE <- rcompare( iris, x)
#' print( COMPARE )
#' print( COMPARE , "Sepal.Length" )
#' 
#' #### Example for ADaM VAD QC
#' # rcompare( AAE , QC_AAE , keys = c("USUBJID" , "AESEQ"))
#' 
#' #### Sample data frames
#' DF1 <- data.frame(
#'     id = c(1,2,3,4,5,6),
#'     v1 = letters[1:6],
#'     v2 = c(NA , NA , 1 , 2 , 3 , NA)
#' )
#' 
#' DF2 <- data.frame(
#'     id = c(1,2,3,4,5,7),
#'     v1 = letters[1:6],
#'     v2 = c(NA , NA , 1 , 2 , NA , NA),
#'     v3 = c(NA , NA , 1 , 2 , NA , 4)
#' )
#' rcompare(DF1 , DF1 , keys = "id")
#' rcompare(DF1 , DF2 , keys = "id")
#' @export
rcompare <- function (base , compare , keys = NULL,
                      suppress_warnings = F, outfile = NULL,
                      tolerance = sqrt(.Machine$double.eps),
                      scale = NULL){
    
    BASE = base
    COMP = compare
    KEYS = keys
    SUPWARN = suppress_warnings
    
    ### Initatiate output object
    COMPARE <- list()
    
    
    
    ### If no key is suplied match values based upon row number
    if (is.null(KEYS)){
        BASE[["..ROWNUMBER.."]] <-  1:nrow(BASE) 
        COMP[["..ROWNUMBER.."]] <-  1:nrow(COMP) 
        KEYS  <- "..ROWNUMBER.."
    }
    
    ## Check tolerance and scale are numeric
    
    if (!is.numeric(tolerance)) {
        stop("'tolerance' should be numeric")
    }
    
    if (!is.numeric(scale) && !is.null(scale)) {
        stop("'scale' should be numeric or NULL")
    }
    

    #################
    #
    # Check essential variable properties (class & mode)
    # 
    
    COMPARE[["UnsupportedColsBase"]] <- construct_issue(
        value = identify_unsupported_cols(BASE) , 
        message  = "There are columns in BASE with unsupported modes !!" , 
        order = 1
    )
    

    COMPARE[["UnsupportedColsComp"]] <- construct_issue(
        value = identify_unsupported_cols(COMP) , 
        message  = "There are columns in COMPARE with unsupported modes !!" , 
        order = 2
    )
    
    
    
    
    COMPARE[["VarModeDiffs"]] <- construct_issue(
        value = identify_mode_differences( BASE, COMP ) ,
        message = "There are columns in BASE and COMPARE with different modes !!",
        order = 3
    )
    
    
    COMPARE[["VarClassDiffs"]] <- construct_issue(
        value = identify_class_differences(BASE, COMP) ,
        message = "There are columns in BASE and COMPARE with different classes !!",
        order = 4
    )
    
    
    
    exclude_cols <- c(
        COMPARE[["UnsupportedColsBase"]]$VARIABLE , 
        COMPARE[["UnsupportedColsComp"]]$VARIABLE,
        COMPARE[["VarClassDiffs"]]$VARIABLE,
        COMPARE[["VarModeDiffs"]]$VARIABLE
    )
    
    #################
    #
    # Check Validity of Keys
    # 
    
    BASE_key_count <- identify_properties(BASE) 
    BASE_key_count <- BASE_key_count[ 
        BASE_key_count[["VARIABLE"]] %in% KEYS,,
        drop=FALSE
    ] %>% 
        nrow
    
    COMP_key_count <- identify_properties(COMP) 
    COMP_key_count <- COMP_key_count[ 
        COMP_key_count[["VARIABLE"]] %in% KEYS,,
        drop=FALSE
    ] %>% 
        nrow
    
    
    if ( BASE_key_count != length(KEYS)  ){
        stop( "BASE is missing variables specified in KEYS")
    }
    
    if ( COMP_key_count != length(KEYS) ){
        stop( "COMP is missing variables specified in KEYS")
    }
    
    if( any(KEYS %in% exclude_cols)){
        stop("KEYS are either an invalid or contain different modes between BASE and COMP")
    }
    
    #################
    #
    # Check Attributes
    # 
    
    COMPARE[["AttribDiffs"]] <- construct_issue(
        value = identify_att_differences(BASE,  COMP ,  exclude_cols)  ,
        message = "There are columns in BASE and COMPARE with differing attributes !!",
        order = 5
    )
    
    
    #################
    #
    # Check data
    # 
    
    #  Check the provided by groups define unique rows
    if ( !has_unique_rows(BASE , KEYS) ){
        stop( "BY variables in BASE do not result in unique observations")
    }
    
    if ( !has_unique_rows(COMP , KEYS) ){
        stop( "BY variables in COMPARE do not result in unique observations")
    }
    
    BASE <- factor_to_character(BASE , KEYS)
    COMP <- factor_to_character(COMP , KEYS)
    
    COMPARE[["ExtRowsBase"]] <- construct_issue(
        value = identify_extra_rows(  BASE, COMP,   KEYS )   ,
        message = "There are rows in BASE that are not in COMPARE !!",
        order = 6
    )
    
    COMPARE[["ExtRowsComp"]] <- construct_issue(
        value = identify_extra_rows(  COMP, BASE,   KEYS )   ,
        message = "There are rows in COMPARE that are not in BASE !!",
        order = 7
    )
    
    
    COMPARE[["ExtColsBase"]] <- construct_issue(
        value =  identify_extra_cols(BASE,  COMP)   ,
        message = "There are columns in BASE that are not in COMPARE !!",
        order = 8
    )
    
    COMPARE[["ExtColsComp"]] <- construct_issue(
        value =  identify_extra_cols(COMP,  BASE)   ,
        message = "There are columns in COMPARE that are not in BASE !!",
        order = 9
    )
    
    
    COMPARE[["VarDiffs"]] <- construct_issue(
        value =  identify_differences(BASE, COMP , KEYS, exclude_cols, tolerance = tolerance, scale = scale) ,
        message = "",
        order = 11,
        add_class = 'issue_list'
    ) 
    
    
    ### Summarise the number of mismatching rows per variable
    if ( length(COMPARE[["VarDiffs"]] ) ){
        VALUES <- COMPARE[["VarDiffs"]]
        VALUE  <- sapply( VALUES , nrow )
    } else {
        VALUE <- 0
    }
    
    COMPARE[["NumDiff"]] <- construct_issue(
        value = convert_to_issue(VALUE), 
        message = "Not all Values Compared Equal",
        order = 10
    )
    
    
    #### Check for issues
    getorder <- map_dbl(COMPARE, function(x) attr(x, "order")) %>%
        order()
    COMPARE <- COMPARE[getorder]
    
    ISSUE_MSGS <- map_chr(COMPARE, function(x) get_issue_message(x) )
    
    ISSUE_MSGS <- ISSUE_MSGS[!ISSUE_MSGS == ""] %>% paste(collapse ='\n')
    
    if( str_length(ISSUE_MSGS) != 0 ){
        if(!SUPWARN) warning( c("\n" , ISSUE_MSGS))
        COMPARE[["Issue_fl"]] <- TRUE
    } else {
        COMPARE[["Issue_fl"]] <- FALSE
    }
    
    class(COMPARE) <- c("rcompare" , "list") 
    
    
    if (!is.null(outfile)){
        sink(outfile)
        print(COMPARE )
        sink()
    }
    
    return(COMPARE)
}






##### Example of use
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
# rcompare(DF1 , DF1 , keys = "id")
# rcompare(DF1 , DF2 , keys = "id")
