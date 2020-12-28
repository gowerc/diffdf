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