


check_extra_cols <- function(df1, df2, name, message){
    
    extra <- names(df1)[! names(df1) %in% names(df2)]

    dat <- data.table("Columns" = extra)
    
    CR <- checkResult$new(
        name = name,
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
        "There are columns in BASE that do not appear in COMPARE"
    )
}

check_extra_cols_comp <- function(base, comp, keys, opts){
    check_extra_cols(
        comp,
        base,
        "ExtraColsComp",
        "There are columns in COMPARE that do not appear in BASE"
    )
}

