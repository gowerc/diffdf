#' make a check for diffdf
#' 
#' @details 
#'  Creates a check with the correct structure for diffdf. Function factory
#'  so argument inputs are contained in the function environment
#'  
#' @inheritParams add_check 

make_check <- function(check_fun, name, message){
    function(base, comp, keys, opts){
        out <- check_fun(base, comp, keys, opts)
        
        disp <- display$new(
            title = out$display_title,
            body = out$display_body
        )
        
        
        CR <- checkResult$new(
            name = name,
            display = disp, 
            result = ifelse(out$result, "Passed", "Failed"), 
            message = message, 
            data = out$data,
            exclude_rows = out$exclude_rows,
            exclude_cols = out$exclude_cols
        )
        return(CR)
    }
}


#' Add a check to diffdf checks
#' 
#' @details
#'  diffdf runs a number of standard checks. This function allows you to
#'  add additional checks to it. The check can either be added at the start of 
#'  all checks or at the end. Note that checks at the end will be performed on a
#'  narrowed down version of the base/comparison datasets where all non 
#'  compatible columns have been removed.
#' 
#' 
#' 
#' 
#' 
#' @param check_fun A function which conducts the check. This function should
#'  take arguments base, comp, keys and opts. It must return a list with 
#'  elements:
#'  \itemize{
#'  \item{result: }{A logical which indicates if the test is passed}
#'  \item{data: }{The data returned by the test, to be passed to the user}
#'  \item{display_title: }{ The title to be displayed when the output of 
#'   this test is printed}
#'  \item{display_body: }{The body to be displayed when the output of this test 
#'    is printed. Empty list if test passed}
#'  \item{exclude_cols: }{Optional vector of columns to exclude from future 
#'   tests}
#'  \item{exclude_rows: }{Optional named list of rows to exclude in base 
#'   and compare. Each element should be a vector with row numbers to exclude}
#'  }
#' @param  name A name for this test. This will be used to identify it in the 
#'  list returned to the user
#' @param message The message for diffdf to display if the test fails
#' @param end A logical indicating whether this check should be done last or 
#'  first. FALSE by default
#' @export
#' @return invisibly NULL: check is added to diffdf 
#' 
#' @examples 
#'  # artificial example where I don't want base to have more than 10 rows!
#'  row_checker <- function(base, comp, keys, opts){
#'   pass <- TRUE
#'   data <- NULL
#'   body <- list()
#'   if(nrow(base)>10){
#'    pass <- FALSE
#'    data <- base[-(1:10),]
#'    body <- paste0(nrow(base) - 10, " additional rows")
#'   }
#'   list(
#'     result = pass,
#'     data = data,
#'     display_title = "Too many rows in Base!",
#'     display_body = body,
#'     exclude_rows = if(pass) NULL else list(base = seq(11, nrow(base)), 
#'      compare = NULL),
#'     exclude_cols = NULL 
#'   
#'   )
#'  }
#'  #test added to end
#'  add_check(row_checker, "rowlimit", "row limit met!")
#'  iris2 <- iris
#'  iris2$Sepal.Length[11] <- 44
#'  diffdf(iris, iris2)
#'  add_check(row_checker,"rowlimit2", "row limit met again!", end = FALSE)
#'  diffdf(iris, iris2)
#'  
#'  
add_check <- function(check_fun,
                       name,
                       message,
                       end = TRUE){
    stopifnot(is.function(check_fun))
    stopifnot(all(names(formals(row_checker)) == 
                      c("base", "comp", "keys", "opts")))
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.character(message), length(message) == 1)
    stopifnot(is.logical(end))
    
    new_fun <- make_check(check_fun, name, message)
    
    diffdf_data$add_check(new_fun,
    end)
    invisible(NULL)
}

#' @export
reset_checks <- function(){
    diffdf_data$reset_checks()
}


