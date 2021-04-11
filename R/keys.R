



generate_keys <- function(base, comp){
    stopifnot(
        is.data.table(base),
        is.data.table(comp)
    )
    keyname <- generate_keyname(base, comp)
    base[, (keyname) :=  seq_len(nrow(base)) ]
    comp[, (keyname) :=  seq_len(nrow(comp)) ]
    return(keyname)
}




#'generate_keyname
#'
#'Function to generate a name for the keys if not provided
#'
#'@param BASE base dataset
#'@param COMP comparison dataset
#'@param replace_names a vector of replacement names. Used for recursion, should be edited in function for clarity
#'
generate_keyname <- function(
    BASE, 
    COMP, 
    replace_names = c("..ROWNUMBER..", "..RN..", "..ROWN..", "..N..")
){
    
    if ( class(replace_names) != "character"){
        stop( "replace_names is not a character vector")
    }
    
    if (length(replace_names) == 0) {
        stop("All default row names are in use in BASE/COMPARE. Please provide a KEY argument")
    }
    
    key_name <- replace_names[1]
    
    if (!is.null(BASE[[key_name]]) | !is.null( COMP[[key_name]])){
        key_name <- generate_keyname(BASE, COMP, replace_names[-1])
    }
    
    return(key_name)
    
}




assert_equal_type <- function(varname, base, comp, fun, funname){
    base_val <- paste0(fun(base[[varname]]), collapse = ", ")
    comp_val <- paste0(fun(comp[[varname]]), collapse = ", ")
    if( !identical(base_val, comp_val)){
        stop(
            sprintf( 
                "Key variable '%s' has a different '%s' between Base and Compare:\nBase = %s\nComp = %s",
                varname,
                funname,
                base_val,
                comp_val
            )
        )
    }
}


#' has_unique_rows
#' 
#' Check if a data sets rows are unique
#' @param DAT input data set (data frame)
#' @param KEYS Set of keys which should be unique
has_unique_rows <- function(DAT , KEYS){
    DUPS <- duplicated(DAT[, KEYS, with = FALSE])  
    NDUPS <- sum( DUPS)
    return( NDUPS == 0 )
}



assert_valid_keys <- function(base, comp, keys){
    keys_in_base <- keys %in% names(base)
    keys_in_comp <- keys %in% names(comp)
    if( !all(keys_in_base)){
        stop("Base is missing the following keys:\n", paste(keys[!keys_in_base], collapse = "\n"))
    }
    if( !all(keys_in_comp)){
        stop("Compare is missing the following keys:\n", paste(keys[!keys_in_comp], collapse = "\n"))
    } 
    for( key in keys){
        assert_equal_type(key, base, comp, typeof, "type")
        assert_equal_type(key, base, comp, mode, "mode")
        assert_equal_type(key, base, comp, class, "class")
    }
    
    if ( !has_unique_rows(base , keys) ){
        stop( "BY variables in BASE do not result in unique observations")
    }
    
    if ( !has_unique_rows(comp , keys) ){
        stop( "BY variables in COMPARE do not result in unique observations")
    }
}













