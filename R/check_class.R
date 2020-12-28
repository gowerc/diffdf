


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
    
    dat2 <- stats::setNames(dat2, c("Variable", "Base", "Compare"))
   
    disp <- display$new(
        title = "Class Mismatches",
        body = list(dat2)
    )
    
    CR <- checkResult$new(
        name = "Class",
        display = disp,
        result = ifelse(all(!KEEP), "Passed", "Failed"), 
        message = "There are columns in Base and Compare with different classes", 
        data = dat2, 
        exclude_cols = dat2[["Variable"]]
    )
    
    return(CR)
}