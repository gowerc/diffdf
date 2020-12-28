


check_extra_cols <- function(df1, df2, name, message, dname){
    
    extra <- names(df1)[! names(df1) %in% names(df2)]

    dat <- data.table("Columns" = extra)
    
    disp <- display$new(
        title = paste0("Extra Columns in ", dname),
        body = list(dat)
    )
    
    CR <- checkResult$new(
        name = name,
        display = disp,
        result = ifelse(length(extra) == 0, "Passed", "Failed"), 
        message = message, 
        data = dat, 
        exclude_cols = extra
    )
    
    return(CR)
}

check_extra_cols_base <- function(base, comp, keys, opts){
    check_extra_cols(
        base,
        comp,
        "ExtraColsBase",
        "There are columns in Base that do not appear in Compare",
        "Base"
    )
}

check_extra_cols_comp <- function(base, comp, keys, opts){
    check_extra_cols(
        comp,
        base,
        "ExtraColsComp",
        "There are columns in Compare that do not appear in Base",
        "Compare"
    )
}

