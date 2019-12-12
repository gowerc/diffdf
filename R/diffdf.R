#' diffdf
#' @description  
#' Compares 2 dataframes and outputs any differences.
#' @param base input dataframe
#' @param compare comparison dataframe
#' @param keys vector of variables (as strings) that defines a unique row in the base and compare dataframes
#' @param config A list containing options for diffdf. Will call \code{\link{diffdf_get_options}}. This can
#'  be updated using \code{\link{diffdf_set_options}}
#' @examples
#' x <- subset( iris,  -Species)
#' x[1,2] <- 5
#' COMPARE <- diffdf( iris, x)
#' print( COMPARE )
#' print( COMPARE , "Sepal.Length" )
#' 
#' #### Sample data frames
#' 
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
#' 
#' diffdf(DF1 , DF1 , keys = "id")
#' 
#' # We can control matching with scale/location for example:
#' 
#' DF1 <- data.frame(
#'     id = c(1,2,3,4,5,6),
#'     v1 = letters[1:6],
#'     v2 = c(1,2,3,4,5,6)
#' )
#' DF2 <- data.frame(
#'     id = c(1,2,3,4,5,6),
#'     v1 = letters[1:6],
#'     v2 = c(1.1,2,3,4,5,6)
#' )
#' 
#' diffdf(DF1 , DF2 , keys = "id")
#' diffdf_set_options(tolerance = 0.2)
#' diffdf(DF1 , DF2 , keys = "id")
#' diffdf_set_options(tolerance = 0.2, scale = 10)
#' diffdf(DF1 , DF2 , keys = "id")
#'  
#' # We can use strict_factor to compare factors with characters for example:
#' 
#' DF1 <- data.frame(
#'     id = c(1,2,3,4,5,6),
#'     v1 = letters[1:6],
#'     v2 = c(NA , NA , 1 , 2 , 3 , NA), 
#'     stringsAsFactors = FALSE
#' )
#' 
#' DF2 <- data.frame(
#'     id = c(1,2,3,4,5,6),
#'     v1 = letters[1:6],
#'     v2 = c(NA , NA , 1 , 2 , 3 , NA)
#' )
#' diffdf_set_options(strict_factor = TRUE)
#' diffdf(DF1 , DF2 , keys = "id", strict_factor = TRUE)
#' diffdf_set_options(strict_factor = FALSE)
#' diffdf(DF1 , DF2 , keys = "id", strict_factor = FALSE)
#' @export
diffdf <- function (
    base , 
    compare , 
    keys = NULL, 
    config = diffdf_get_options()

){
    
    warnings <- config$warnings
    strict_numeric <- config$strict_numeric
    strict_factor <- config$strict_factor
    file <- config$file 
    tolerance <- config$tolerance
    scale <- config$scale
    
    setDTthreads(1)
    BASE = as.data.table(base)
    COMP = as.data.table(compare)
    KEYS = keys
    WARN = warnings
    
    
    ### Initatiate output object
    COMPARE <- list()
    class(COMPARE) <- c("diffdf" , "list") 
    
    is_derived <- FALSE
    
    ### If no key is suplied match values based upon row number
    if (is.null(KEYS)){
        is_derived <- TRUE
        keyname <- generate_keyname(BASE, COMP)
        BASE[[keyname]] <-  1:nrow(BASE) 
        COMP[[keyname]] <-  1:nrow(COMP) 
        KEYS  <- keyname
    }
    attr(COMPARE, 'keys') <- list(value = KEYS, is_derived = is_derived)
    
    
   
    
    
    if ( !has_unique_rows(BASE , KEYS) ){
        stop( "BY variables in BASE do not result in unique observations")
    }
    
    if ( !has_unique_rows(COMP , KEYS) ){
        stop( "BY variables in COMPARE do not result in unique observations")
    }
    
    

    #### Check essential variable properties (class & mode)
    
    COMPARE[["UnsupportedColsBase"]] <- construct_issue(
        value = identify_unsupported_cols(BASE) , 
        message  = "There are columns in BASE with unsupported modes !!" 
    )
    
    
    COMPARE[["UnsupportedColsComp"]] <- construct_issue(
        value = identify_unsupported_cols(COMP) , 
        message  = "There are columns in COMPARE with unsupported modes !!" 
    )

    
    # cast variables if strict is off
    if ( !strict_factor | !strict_numeric ){
        
        casted_df <- cast_variables( 
            BASE = BASE, 
            COMPARE = COMP, 
            ignore_vars = KEYS, 
            cast_integers = !strict_numeric , 
            cast_factors = !strict_factor
        )
        
        BASE <- casted_df$BASE
        COMP <- casted_df$COMP
        
    }
    
    
    COMPARE[["VarModeDiffs"]] <- construct_issue(
        value = identify_mode_differences( BASE, COMP ) ,
        message = "There are columns in BASE and COMPARE with different modes !!"
    )
    
    
    COMPARE[["VarClassDiffs"]] <- construct_issue(
        value = identify_class_differences(BASE, COMP) ,
        message = "There are columns in BASE and COMPARE with different classes !!"
    )
    

    exclude_cols <- c(
        COMPARE[["UnsupportedColsBase"]]$VARIABLE , 
        COMPARE[["UnsupportedColsComp"]]$VARIABLE,
        COMPARE[["VarClassDiffs"]]$VARIABLE,
        COMPARE[["VarModeDiffs"]]$VARIABLE
    )

    ##### Check Validity of Keys
    
    BASE_keys <- names(BASE)[names(BASE) %in% KEYS] 
    COMP_keys <- names(COMP)[names(COMP) %in% KEYS] 
    
    
    if ( length(BASE_keys) != length(KEYS)  ){
        stop( "BASE is missing variables specified in KEYS")
    }
    
    if ( length(COMP_keys) != length(KEYS) ){
        stop( "COMP is missing variables specified in KEYS")
    }
    
    if( any(KEYS %in% exclude_cols)){
        stop("KEYS are either an invalid or contain different modes between BASE and COMP")
    }
    
    ## Remove any excluded columns
    if( !is.null(exclude_cols)){
        BASE <- BASE[, !exclude_cols, with = FALSE]
        COMP <- COMP[, !exclude_cols, with = FALSE]
    }
    

    ##### Check Attributes
    
    COMPARE[["AttribDiffs"]] <- construct_issue(
        value = identify_att_differences(BASE, COMP)  ,
        message = "There are columns in BASE and COMPARE with differing attributes !!"
    )
    
    
    ##### Check data
    
    BASE <- factor_to_character(BASE, KEYS)
    COMP <- factor_to_character(COMP, KEYS)
    
    COMPARE[["ExtRowsBase"]] <- construct_issue(
        value = identify_extra_rows(BASE, COMP, KEYS ),
        message = "There are rows in BASE that are not in COMPARE !!"
    )
    
    
    COMPARE[["ExtRowsComp"]] <- construct_issue(
        value = identify_extra_rows(COMP, BASE, KEYS ),
        message = "There are rows in COMPARE that are not in BASE !!"
    )

    
    
    COMPARE[["ExtColsBase"]] <- construct_issue(
        value =  identify_extra_cols(BASE, COMP),
        message = "There are columns in BASE that are not in COMPARE !!"
    )
    
    
    COMPARE[["ExtColsComp"]] <- construct_issue(
        value =  identify_extra_cols(COMP, BASE),
        message = "There are columns in COMPARE that are not in BASE !!"
    )
   
    ## Remove extra columns
    base_remove <- COMPARE[["ExtColsBase"]][["COLUMNS"]]
    comp_remove <- COMPARE[["ExtColsComp"]][["COLUMNS"]]
    
    if( !is.null(base_remove)) BASE <- BASE[,!base_remove, with = FALSE]
    if( !is.null(comp_remove)) COMP <- COMP[,!comp_remove, with = FALSE]
    
     
    
    VALUE_DIFFERENCES <- identify_differences(
        BASE, COMP , KEYS, tolerance = tolerance, scale = scale
    )

    
    ## Summarise the number of mismatching rows per variable
    if ( length(VALUE_DIFFERENCES) ){
        NDIFF  <- sapply( VALUE_DIFFERENCES , nrow)
        COMPARE[["NumDiff"]] <- construct_issue(
            value = convert_to_issue(NDIFF),
            message = "Not all Values Compared Equal"
        )
    }


    for ( i in names(VALUE_DIFFERENCES) ){
        COMPARE[[ paste0( "VarDiff_", i)]] <- construct_issue(
            value = VALUE_DIFFERENCES[[i]] ,
            message = ""
        )
    }
    
    ## Get all issue messages, remove blank message, and collapse into single string
    ISSUE_MSGS <- sapply(COMPARE, function(x) get_issue_message(x))
    ISSUE_MSGS <- ISSUE_MSGS[ ISSUE_MSGS != ""]
    
    if( length(ISSUE_MSGS) != 0 ){
        if(WARN) {
            ISSUE_MSGS <- paste(ISSUE_MSGS, collapse ='\n' )
            warning( c("\n" , ISSUE_MSGS))
        }
    } 
    
    
    if (!is.null(file)){
        x <- print(COMPARE , as_string = TRUE)
        
        tryCatch(
            {
                sink(file)
                cat(x, sep = "\n")
                sink()
            },
            warning = function(w){
                sink() 
                warning(w)
            },
            error = function(e){
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
#' x <- diffdf( iris, iris )
#' diffdf_has_issues(x)
#' 
#' # Example with issues
#' iris2 <- iris
#' iris2[2,2] <- NA
#' x <- diffdf( iris , iris2 , warnings = FALSE)
#' diffdf_has_issues(x)
#' @export
diffdf_has_issues <- function(x){
    if (  class(x)[[1]] != "diffdf" )  stop( "x is not an diffdf object")
    return( length(x) != 0 ) 
}

