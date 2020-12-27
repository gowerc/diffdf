


check_class <- function(base, comp, keys, opts){
    
    dat <- merge(
        x = get_properties(base),
        y = get_properties(comp),
        by = "VARIABLE",
        all = TRUE, 
        sort = TRUE,
        suffixes =  c(".BASE", ".COMP")
    ) 
    
    dat <- dat[, c("VARIABLE", "CLASS.BASE" , "CLASS.COMP")] 
    
    KEEP <- !mapply( 
        identical,
        dat[["CLASS.BASE"]], 
        dat[["CLASS.COMP"]] 
    )
    
    dat2 <- dat[KEEP]
    
    
    CR <- checkResult$new(
        name = "Class",
        result = ifelse(length(KEEP) == 0, "Passed", "Failed"), 
        message = "There are columns in BASE and COMPARE with different classes", 
        data = dat2, 
        exclude_cols = dat2[["VARIABLE"]]
    )
    
    return(CR)
}