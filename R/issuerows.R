




#' diffdf_issuerows
#' 
#' This function takes a diffdf object and a dataframe and subsets
#' the dataframe for problem rows as identified in the comparison object.
#' If \code{vars} has been specified only issue rows associated with those
#' variable(s) will be returned. 
#' @param df dataframe to be subsetted
#' @param diff diffdf object
#' @param vars (optional) character vector containing names of issue variables to subset dataframe 
#' on. A value of NULL (default) will be taken to mean available issue variables.
#' @examples 
#' iris2 <- iris
#' for ( i in 1:3) iris2[i,i] <- 99
#' x <- diffdf( iris , iris2, suppress_warnings = TRUE)
#' diffdf_issuerows( iris , x)
#' diffdf_issuerows( iris2 , x)
#' diffdf_issuerows( iris2 , x , vars = "Sepal.Length")
#' diffdf_issuerows( iris2 , x , vars = c("Sepal.Length" , "Sepal.Width"))
#' @details 
#' Note that diffdf_issuerows can be used to subset against any dataframe. The only
#' requirement is that the original variables specified in the keys argument to diffdf
#' are present on the dataframe you are subsetting against. However please note that if 
#' no keys were specified in diffdf then the row number is used. This means using 
#' diffdf_issuerows without a keys against an arbitrary dataset can easily result in 
#' nonsense rows being returned. It is always recommended to supply keys to diffdf.
#' @export
diffdf_issuerows <- function( df , diff, vars = NULL){    

    if ( class(diff)[[1]] != "diffdf") {
        stop("diff should be an diffdf object")
    }
    
    KEYS_ATT = attr(diff, "keys")
    
    if( is.null(KEYS_ATT) ) {
        stop("diff is missing the keys attribute")
    }
    
    issue_vars <- names(diff)[grep( "^VarDiff_", names(diff))]
    
    if ( is.null(vars)){
        vars <- issue_vars
    }  else {
        vars <- paste0("VarDiff_" , vars)
    }
    
    if ( length(issue_vars) == 0 | sum( vars %in% issue_vars) == 0){
        return(df[FALSE,])
    }
    
    KEEP <- mapply(
        FUN      = get_issue_dataset, 
        issue    = vars, 
        diff     = list(diff) , 
        SIMPLIFY = F
    )
    
    KEEP <- recursive_reduce( KEEP , rbind)
    KEEP <- KEEP[!duplicated(KEEP),]
    
    if ( KEYS_ATT$is_derived ){
        df[[KEYS_ATT$value]] <- 1:nrow(df)
    }

    keys <- KEYS_ATT$value
    
    if ( any( ! keys %in% names(df))){
        stop("df does not contain all variables specified as keys in diff")
    }
    
    RET <- merge( 
        x = df,
        y = KEEP,
        sort = TRUE
    )  
    
    RET <- RET[do.call("order", RET[keys]), ]
    
    if ( KEYS_ATT$is_derived ){
        keep_vars <- !names(RET) %in% KEYS_ATT$value
        RET <- RET[, keep_vars , drop = FALSE]
    }
    
    return(RET)
}




#' get_issue_dataset
#' 
#' Internal function used by diffdf_issuerows to extract the dataframe
#' from each a target issue. In particular it also strips off any 
#' non-key variables
#' @param issue name of issue to extract the dataset from diff 
#' @param diff diffdf object which contains issues
get_issue_dataset <- function(issue, diff){
    issue_df <- diff[[issue]]
    keep <- names(issue_df)[ !(names(issue_df) %in% c("BASE", "COMPARE", "VARIABLE"))]
    issue_df[,keep , drop=FALSE]
}
