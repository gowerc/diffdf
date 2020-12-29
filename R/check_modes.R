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
        dat[["MODE.BASE"]], 
        dat[["MODE.COMP"]] 
    )
    
    dat2 <- dat[KEEP]
    
    disp <- display$new(
        title = "Mode Mismatches",
        body = list(dat2)
    )
    
    CR <- checkResult$new(
        name = "Mode",
        display = disp,
        result = ifelse( any(KEEP), "Failed", "Passed"), 
        message = "There are columns in Base and Compare with different modes", 
        data = dat2, 
        exclude_cols = dat2[["VARIABLE"]]
    )
    
    return(CR)
}