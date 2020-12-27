check_modes <- function(base, comp, keys, opts){
    
    dat <- merge(
        x = get_properties(base),
        y = get_properties(comp),
        by = "VARIABLE",
        all = TRUE, 
        sort = TRUE,
        suffixes =  c(".BASE", ".COMP")
    ) 
    
    dat <- dat[, c("VARIABLE", "MODE.BASE", "MODE.COMP")] 
    
    KEEP <- !mapply( 
        identical,
        dat[["CLASS.BASE"]], 
        dat[["CLASS.COMP"]] 
    )
    
    dat2 <- dat[KEEP]
    
    CR <- checkResult$new(
        name = "Mode",
        result = ifelse(length(KEEP) == 0, "Passed", "Failed"), 
        message = "There are columns in BASE and COMPARE with different modes", 
        data = dat2, 
        exclude_cols = dat2[["VARIABLE"]]
    )
    
    return(CR)
}