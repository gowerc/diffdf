#' @import tibble
#' @import data.table
diffdf <- function(base, comp, keys = NULL, opts = NULL){
    
    setDTthreads(1)
    base <- as.data.table(base)
    comp <- as.data.table(comp)
    base <- data.table::copy(base)
    comp <- data.table::copy(comp)
    
    if (is.null(keys)){
        keys <- generate_keys(base, comp)
    }
    
    assert_valid_keys(base, comp, keys)
    
    main <- diffMain$new(base, comp, keys, opts)
    
    #main$perform_check(check_df_class)
    #main$perform_check(check_df_attrib)
    
    main$perform_check(check_extra_rows_base)
    main$perform_check(check_extra_rows_comp)
    main$perform_check(check_extra_cols_base)
    main$perform_check(check_extra_cols_comp)
    
    #main$perform_check(check_col_order)
    #main$perform_check(check_row_order)
    
    main$perform_check(check_modes)
    main$perform_check(check_class)
    main$perform_check(check_attributes)
     
    main$perform_check(check_values)    
    
    return(main$diff_result)
}




