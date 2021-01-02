#' diffdf
#' @description  
#' Compares 2 dataframes and provides details on any differences found
#' @param base input dataframe
#' @param compare comparison dataframe
#' @param keys vector of variables (as strings) that defines a unique row in the base and compare dataframes
#' @param opts a named list of arguments passed onto diffopts. See \code{\link[diffdf]{diffopts}}
#' @param ... arguments passed onto diffopts (takes precedence over `opts`). See \code{\link[diffdf]{diffopts}}
#' @examples
#' library(diffdf)
#' 
#' dat1 <- data.frame(
#'     id = c(1, 2, 3),
#'     var1 = c(4, 5, 6),
#'     var2 = c(1L, 2L, 3L),
#'     var3 = factor(c("a", "b", "c")),
#'     var4 = c(1, 2, 3)
#' )
#' 
#' dat2 <- data.frame(
#'     id = c(0, 2, 3),
#'     var1 = c(4, 5, 6),
#'     var2 = c(1, 2, 3),
#'     var3 = factor(c("a", "b", "d"))
#' )
#' 
#' x <- diffdf(dat1, dat2, keys = c("id")) 
#' print(x, type = "html")
#' summary(x)
#' 
#' @export
diffdf <- function(base, compare, keys = NULL, opts = NULL, ...){
    setDTthreads(1)
    opts <- merge_options(opts, ...)
    
    main <- diffMain$new(base, compare, keys, opts)
    
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




