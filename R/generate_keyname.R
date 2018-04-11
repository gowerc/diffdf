#'generate_keyname
#'
#'Function to generate a name for the keys if not provided
#'
#'@param BASE base dataset
#'@param COMP comparison dataset
#'@param replace_names a list of replacement names. Used for recursion, should be edited in function for clarity
#'
generate_keyname <- function(BASE, COMP, replace_names = list("..ROWNUMBER..", "..RN..", "..ROWN..", "..N..")){
    
    if (length(replace_names) == 0) stop("All default row names are in use in BASE/COMPARE. Please provide a KEY argument")
    
    key_name <- replace_names[[1]]
    if (!is.null(BASE[[key_name]]) | !is.null( COMP[[key_name]])){
        replace_names[[1]] <- NULL
        key_name <- generate_keyname(BASE, COMP, replace_names)
    }
    key_name
    
}