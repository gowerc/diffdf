#' diffdf
#' 
#' Description - TODO
#' @param base TODO
#' @param comp TODO
#' @param keys TODO
#' @param opts TODO
#' @param ... TODO
#' 
#' @export
diffdf <- function(base, comp, keys = NULL, opts = NULL, ...){
    setDTthreads(1)
    
    opts <- merge_options(opts, ...)
    
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
    
    return(main$get_result())
}




