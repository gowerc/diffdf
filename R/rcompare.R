
#' Compares dataframes
#' @description  Emulates proc compare from SAS
#' Compares 2 data frames and outputs any differences.
#' @param base input dataframe
#' @param compare comparison dataframe
#' @param keys vector of variables (as strings) that defines a unique row in the base and compare dataframes
#' @param suppress_warnings Do you want to suppress warnings? (logical)
#' @param outfile Location and name of outputted file. Leave as NULL to not output a file
#' @import dplyr
#' @importFrom purrr map_dbl
#' @importFrom purrr map_chr
#' @examples
#' # rcompare( AAE , QC_AAE , keys = c("USUBJID" , "AESEQ"))
#' @export
rcompare <- function (base , compare , keys = NULL, suppress_warnings = F, outfile = NULL){

    BASE = base
    COMP = compare
    KEYS = keys
    SUPWARN = suppress_warnings
    
    ### Initatiate output object
    COMPARE <- list()
    
    
    
    ### If no key is suplied match values based upon row number
    if (is.null(KEYS)){
        BASE <- BASE %>% mutate( ..ROWNUMBER.. = row_number())
        COMP <- COMP %>% mutate( ..ROWNUMBER.. = row_number())
        KEYS  <- "..ROWNUMBER.."
    }
    
    
    #################
    #
    # Check essential variable properties (class & mode)
    # 
    
    COMPARE[["UnsupportedColsBase"]] <- issue_basic$new(
        value = identify_unsupported_cols(BASE) ,
        message = "There are columns in BASE with unsupported modes !!",
        order = 1
    )


    
    COMPARE[["UnsupportedColsComp"]] <- issue_basic$new(
        value = identify_unsupported_cols(COMP) ,
        message = "There are columns in COMPARE with unsupported modes !!",
        order = 2
    )
    

    
    COMPARE[["VarModeDiffs"]] <- issue_basic$new(
        value = identify_mode_differences( BASE, COMP ) ,
        message = "There are columns in BASE and COMPARE with different modes !!",
        order = 3
    )
    

    COMPARE[["VarClassDiffs"]] <- issue_basic$new(
        value = identify_class_differences(BASE, COMP) ,
        message = "There are columns in BASE and COMPARE with different classes !!",
        order = 4
    )
    

    
    exclude_cols <- c(
        COMPARE[["UnsupportedColsBase"]]$value$VARIABLE , 
        COMPARE[["UnsupportedColsComp"]]$value$VARIABLE,
        COMPARE[["VarClassDiffs"]]$value$VARIABLE,
        COMPARE[["VarModeDiffs"]]$value$VARIABLE
    )
    
    #################
    #
    # Check Validity of Keys
    # 
    
    BASE_key_count <- identify_properties(BASE) %>% 
        filter( VARIABLE %in% KEYS) %>% 
        nrow
    
    COMP_key_count <- identify_properties(COMP) %>% 
        filter( VARIABLE %in% KEYS) %>% 
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
    
    COMPARE[["AttribDiffs"]] <- issue_basic$new(
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
    
    COMPARE[["ExtRowsBase"]] <- issue_basic$new(
        value = identify_extra_rows(  BASE, COMP,   KEYS )   ,
        message = "There are rows in BASE that are not in COMPARE !!",
        order = 6
    )
    
    COMPARE[["ExtRowsComp"]] <- issue_basic$new(
        value = identify_extra_rows(  COMP, BASE,   KEYS )   ,
        message = "There are rows in COMPARE that are not in BASE !!",
        order = 7
    )
   
    
    COMPARE[["ExtColsBase"]] <- issue_basic$new(
        value =  identify_extra_cols(BASE,  COMP)   ,
        message = "There are columns in BASE that are not in COMPARE !!",
        order = 8
    )
    
    COMPARE[["ExtColsComp"]] <- issue_basic$new(
        value =  identify_extra_cols(COMP,  BASE)   ,
        message = "There are columns in COMPARE that are not in BASE !!",
        order = 9
    )
    

    COMPARE[["VarDiffs"]] <- issue_list$new(
        value =  identify_differences(BASE, COMP , KEYS, exclude_cols) ,
        message = "Not all Values Compared Equal",
        order = 11
    ) 
    
 
    ### Summarise the number of mismatching rows per variable
    if ( length(COMPARE[["VarDiffs"]]$value ) ){
        VALUES <- map( COMPARE[["VarDiffs"]]$value , "value" )
        VALUE <- sapply( VALUES , nrow )
    } else {
        VALUE <- 0
    }
    
    COMPARE[["NumDiff"]] <- issue_vector$new(
        value = VALUE, 
        message = "",
        order = 10
    )
    
    
    #### Check for issues
    getorder <- map_dbl(COMPARE, 'order') %>% order
    COMPARE <- COMPARE[getorder]
    
    ISSUES <- map_chr(COMPARE, function(x) x$get_issue_message() )

    ISSUES <- ISSUES[!ISSUES == ""] %>% paste(collapse ='\n')
    
    if( str_length(ISSUES) != 0 ){
        if(!SUPWARN) warning( c("\n" , ISSUES))
        COMPARE[["Issues"]] <- TRUE
    } else {
        COMPARE[["Issues"]] <- FALSE
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
