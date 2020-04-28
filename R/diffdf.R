#' diffdf
#' @description  
#' Compares 2 dataframes and outputs any differences.
#' @param base input dataframe
#' @param compare comparison dataframe
#' @param keys vector of variables (as strings) that defines a unique row in the base and compare dataframes
#' @param strict_numeric Flag for strict numeric to numeric comparisons (default = TRUE). If False diffdf will cast integer to double where required for comparisons. Note that variables specified in the keys will never be casted.
#' @param strict_factor Flag for strict factor to character comparisons (default = TRUE). If False diffdf will cast factors to characters where required for comparisons. Note that variables specified in the keys will never be casted.
#' @param warnings Do you want to display warnings? (logical) (default = TRUE)
#' @param tolerance Set tolerance for numeric comparisons. Note that comparisons fail if (x-y)/scale > tolerance.
#' @param scale Set scale for numeric comparisons. Note that comparisons fail if (x-y)/scale > tolerance. Setting as NULL is a slightly more efficient version of scale = 1. 
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
#' diffdf(DF1 , DF2 , keys = "id", tolerance = 0.2)
#' diffdf(DF1 , DF2 , keys = "id", scale = 10, tolerance = 0.2)
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
#' 
#' diffdf(DF1 , DF2 , keys = "id", strict_factor = TRUE)
#' diffdf(DF1 , DF2 , keys = "id", strict_factor = FALSE)
#' @export
diffdf <- function (
    base , 
    compare , 
    keys = NULL, 
    warnings = TRUE, 
    strict_numeric = TRUE,
    strict_factor = TRUE,
    file = NULL,
    tolerance = sqrt(.Machine$double.eps),
    scale = NULL
){
    
    call <- match.call()
    
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
    
    
    if (!is.numeric(tolerance)) {
        stop("'tolerance' should be numeric")
    }
    
    if (!is.numeric(scale) && !is.null(scale)) {
        stop("'scale' should be numeric or NULL")
    }
    
    
    
    if ( !has_unique_rows(BASE , KEYS) ){
        stop( "BY variables in BASE do not result in unique observations")
    }
    
    if ( !has_unique_rows(COMP , KEYS) ){
        stop( "BY variables in COMPARE do not result in unique observations")
    }
    
    

    #### Check essential variable properties (class & mode)

    COMPARE[["UnsupportedColsBase"]] <- construct_check(
        name = "Do all columns in BASE have supported modes?",
        error_message = "There are columns in BASE with unsupported modes!" , 
        value =  identify_unsupported_cols(BASE), 
        ord = 10
    )
    
    COMPARE[["UnsupportedColsComp"]] <- construct_check(
        name = "Do all columns in COMPARE have supported modes?",
        error_message = "There are columns in COMPARE with unsupported modes!" , 
        value =  identify_unsupported_cols(COMP), 
        ord = 20 
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
    
    COMPARE[["VarModeDiffs"]] <- construct_check(
        name = "Do columns in BASE and COMPARE have the same mode?",
        value = identify_mode_differences( BASE, COMP ) ,
        error_message = "There are columns in BASE and COMPARE with different modes !", 
        ord = 30
    )
    
    
    COMPARE[["VarClassDiffs"]] <- construct_check(
        name = "Do columns in BASE and COMPARE have the same class?",
        value = identify_class_differences(BASE, COMP) ,
        error_message = "There are columns in BASE and COMPARE with different classes !", 
        ord = 40
    )
    

    exclude_cols <- c(
        get_value(COMPARE[["UnsupportedColsBase"]])$VARIABLE , 
        get_value(COMPARE[["UnsupportedColsComp"]])$VARIABLE,
        get_value(COMPARE[["VarClassDiffs"]])$VARIABLE,
        get_value(COMPARE[["VarModeDiffs"]])$VARIABLE
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
    if( length(exclude_cols) > 0){
        BASE <- BASE[, !exclude_cols, with = FALSE]
        COMP <- COMP[, !exclude_cols, with = FALSE]
    }
    

    ##### Check Attributes
    
    COMPARE[["AttribDiffs"]] <- construct_check(
        name = "Do columns in BASE and COMPARE have the same attributes?",
        value = identify_att_differences(BASE, COMP)  ,
        error_message = "There are columns in BASE and COMPARE with differing attributes !", 
        ord = 50
    )
    
    
    ##### Check data
    
    BASE <- factor_to_character(BASE, KEYS)
    COMP <- factor_to_character(COMP, KEYS)
    
    COMPARE[["ExtRowsBase"]] <- construct_check(
        name = "Do all rows in BASE exist in COMPARE?",
        value = identify_extra_rows(BASE, COMP, KEYS ),
        error_message = "There are rows in BASE that are not in COMPARE !", 
        ord = 60
    )
    
    
    COMPARE[["ExtRowsComp"]] <- construct_check(
        name = "Do all rows in COMPARE exist in BASE?",
        value = identify_extra_rows(COMP, BASE, KEYS ),
        error_message = "There are rows in COMPARE that are not in BASE !", 
        ord = 70
    )

    
    
    COMPARE[["ExtColsBase"]] <- construct_check(
        name = "Do all columns in BASE exist in COMPARE?",
        value =  identify_extra_cols(BASE, COMP),
        error_message = "There are columns in BASE that are not in COMPARE !", 
        ord = 80
    )
    
    
    COMPARE[["ExtColsComp"]] <- construct_check(
        name = "Do all columns in COMPARE exist in BASE?",
        value =  identify_extra_cols(COMP, BASE),
        error_message = "There are columns in COMPARE that are not in BASE !", 
        ord = 90
    )
    
    ## Remove extra columns
    base_remove <- get_value(COMPARE[["ExtColsBase"]])[["COLUMNS"]]
    comp_remove <- get_value(COMPARE[["ExtColsComp"]])[["COLUMNS"]]
    
    if( length(base_remove) > 0) BASE <- BASE[,!base_remove, with = FALSE]
    if( length(comp_remove) > 0) COMP <- COMP[,!comp_remove, with = FALSE]
    
    
    VALUE_DIFFERENCES <- identify_differences(
        BASE, COMP , KEYS, tolerance = tolerance, scale = scale
    )
    
    VALUE_DIFFERENCES_CHECKS <- list()
    for( i in names(VALUE_DIFFERENCES)){
        VALUE_DIFFERENCES_CHECKS[[i]] <- construct_check(
            name = paste0("Do all values in ", i, " compare the same?"),
            value = VALUE_DIFFERENCES[[i]], 
            error_message = paste0("Differences found in ", i)
        )
    }
    
    NUMBER_OF_DIFFERENCES <-  vapply( 
        VALUE_DIFFERENCES_CHECKS, 
        function(x) nrow(get_value(x)) , 
        numeric(1)
    )
    
    VALUE_DIFFERENCES_SUMMARY <- data.frame(
        Variable = names(VALUE_DIFFERENCES_CHECKS),
        `Number of Differences` = NUMBER_OF_DIFFERENCES, 
        stringsAsFactors = FALSE
    )
    VALUE_DIFFERENCES_SUMMARY <- VALUE_DIFFERENCES_SUMMARY[VALUE_DIFFERENCES_SUMMARY[, 2] != 0,]
    
    COMPARE[["Variables"]] <- construct_check_collection(
        name = "Do all values compare equal?",
        value = VALUE_DIFFERENCES_SUMMARY,
        collection = VALUE_DIFFERENCES_CHECKS,
        error_message = "Not all Values Compared Equal", 
        ord = 100
    )
   
    COMPARE[["Summary"]] <- get_diffdf_summary(
        base = base,
        comp = compare,
        keys = KEYS,
        base_name = call$base,
        comp_name = call$comp,
        COMPARE = COMPARE
        
    )
    
    df_ord <- get_ord(COMPARE)
    COMPARE <- COMPARE[order(df_ord)]
    class(COMPARE) <- "diffdf"
    
    ## Get all issue messages, remove blank message, and collapse into single string
    warning_message <- get_error_message(COMPARE)
    if( warning_message != "" & WARN){
        warning( paste0(warning_message, sep = "\n"))
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
    return( get_error_message(x) != "" ) 
}





get_error_message.diffdf <- function(x, ...){
    get_required_errors <- function(x){
        if( get_was_performed(x) & get_is_test(x)){
            if( !get_is_pass(x)){
                return( get_error_message(x))
            }
        }
        return("")
    }
    message <- vapply(x, get_required_errors, character(1))
    message <- message[message != ""]
    paste0(message, collapse = "\n")
}

get_ord.diffdf <- function(x, ...){
    vapply( x , get_ord, numeric(1))
}







