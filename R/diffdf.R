#' diffdf
#' @description  
#' Compares 2 dataframes and provides details on any differences found. 
#' For options related to the output format as well as the ability to create a report
#' please see \code{\link[diffdf]{print.diffResult}}
#' @param base input dataframe
#' @param compare comparison dataframe
#' @param keys vector of variables (as strings) that defines a unique row in the base and compare dataframes
#' @param opts a named list of arguments passed onto diffopts. See \code{\link[diffdf]{diffopts}}
#' @param ... arguments passed onto diffopts (takes precedence over `opts`). See \code{\link[diffdf]{diffopts}}
#' @import data.table 
#' @importFrom stats setNames
#' @examples
#' \dontrun{
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
#' y <- summary(x)
#' names(y)
#' y$Values$var3
#' }
#' @export
diffdf <- function(base, compare, keys = NULL, opts = NULL, ...){
    CALL <- match.call(expand.dots = FALSE)
    setDTthreads(1)
    opts <- merge_options(opts, ...)
    main <- diffMain$new(base, compare, keys, opts, CALL)
    main$perform_checks()
    main$set_result()
    return(main$diff_result)
}
