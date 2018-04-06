




#' dfdiff_issuerows
#' 
#' This function takes a dfdiff object and a dataframe and subsets
#' the dataframe for problem rows as identified in the comparison object.
#' If \code{vars} has been specified only issue rows associated with those
#' variable(s) will be returned. 
#' @param df dataframe to be subsetted
#' @param diff dfdiff object
#' @param vars (optional) character vector containing names of issue variables to subset dataframe 
#' on. A value of NULL (default) will be taken to mean available issue variables.
#' @examples 
#' iris2 <- iris
#' for ( i in 1:3) iris2[i,i] <- 99
#' x <- dfdiff( iris , iris2, suppress_warnings = TRUE)
#' dfdiff_issuerows( iris , x)
#' dfdiff_issuerows( iris2 , x)
#' dfdiff_issuerows( iris2 , x , vars = "Sepal.Length")
#' dfdiff_issuerows( iris2 , x , vars = c("Sepal.Length" , "Sepal.Width"))
#' @details 
#' Note that dfdiff_issuerows can be used to subset against any dataframe. The only
#' requirement is that the original variables specified in the keys argument to dfdiff
#' are present on the dataframe you are subsetting against. However please note that if 
#' no keys are specified in dfdiff then rownumber is used which means using 
#' dfdiff_issuerows against an arbitary dataset can easily result in nonsense rows 
#' being returned. It is always recommended to supply keys to dfdiff.
#' @export
dfdiff_issuerows <- function( df , diff, vars = NULL){    

    if ( class(diff)[[1]] != "dfdiff") {
        stop("diff should be an dfdiff object")
    }
    
    issue_vars <- names(diff)[grep( "^VarDiff_", names(diff))]
    
    if ( length(issue_vars) == 0){
        return(df[1==0,])
    }
    
    if ( is.null(vars)){
        vars <- issue_vars
    }  else {
        vars <- paste0("VarDiff_" , vars)
    }
    
    KEEP <- mapply(get_issue_dataset, vars, diff = list(diff) , SIMPLIFY = F)
    KEEP <- recursive_reduce( KEEP , rbind)
    KEEP <- KEEP[!duplicated(KEEP),]
    
    df[["..ROWNUMBER.."]] <- 1:nrow(df)
    
    keys <- names(KEEP)[ !names(KEEP) %in% c("BASE", "COMPARE")]
    
    if ( any( ! keys %in% names(df))){
        stop("df does not contain all variables specified as keys in diff")
    }
    
    RET <- merge( 
        x = df,
        y= KEEP,
        sort = TRUE
    )  
    RET <- RET[do.call("order", RET[keys]), ]
    
    RET[ ,! names(RET) %in% "..ROWNUMBER..", drop=FALSE]    
}




#' get_issue_dataset
#' 
#' Internal function used by dfdiff_issuerows to extract the dataframe
#' from each a target issue. In particular it also strips off any 
#' non-key variables
#' @param issue name of issue to extract issue from
#' @param diff dfdiff object which contains issues
get_issue_dataset <- function(issue, diff){
    issue_df <- diff[[issue]]
    keep <- names(issue_df)[ !(names(issue_df) %in% c("BASE", "COMPARE", "VARIABLE"))]
    issue_df[,keep , drop=FALSE]
}
