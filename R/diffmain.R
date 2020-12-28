
diffMain <- R6::R6Class(
    classname = "diffMain",
    public = list(
        
        base = NULL,
        comp = NULL,
        keys = NULL,
        opts = NULL,
        diff_result = NULL,
        
        initialize = function(base, comp, keys, opts, ...){
            
            xopts <- diffopts(...)
            for( i in names(opts)) xopts[[i]] <- opts[[i]]
            
            self$diff_result <- diffResult$new(base, comp, keys)
            
            base <- copy(as.data.table(base))
            comp <- copy(as.data.table(comp))
            
            if (is.null(keys)) keys <- generate_keys(base, comp)
            
            assert_valid_keys(base, comp, keys)
            
            self$base <- base
            self$comp <- comp
            self$keys <- keys
            self$opts <- xopts
        },
        
        perform_check = function(check_fun){

            check_result <- check_fun(self$base, self$comp, self$keys, self$opts)
            
            exclude_cols <- check_result$exclude_cols
            exclude_rows_base <- check_result$exclude_rows$base
            exclude_rows_comp <- check_result$exclude_rows$comp
            
            if(!is.null(exclude_cols)){
                self$base <- remove_columns(self$base, exclude_cols)
                self$comp <- remove_columns(self$comp, exclude_cols)
            }
            
            if(!is.null(exclude_rows_base)){
                self$base <- remove_rows(self$base, exclude_rows_base)
            }
            
            if(!is.null(exclude_rows_comp)){
                self$comp <- remove_rows(self$comp, exclude_rows_comp)
            }
            
            self$diff_result$add_checkResult(check_result)
        
            return(invisible())
        },
        
        get_result = function(){
            
            onfailure <- self$opts$onfailure
            failurefun <- switch(onfailure,
                "error" = function(x) stop(x, call. = FALSE),
                "warning" = function(x) warning(x, call. = FALSE),
                "message" = message,
                "nothing" = function(x){invisible()}
            )
            
            x <- Filter(
                function(x) x$result == "Failed",
                self$diff_result$checks
            )
            
            xmsg <- lapply(x, function(x) c(x$message, "\n"))
   
            if(length(xmsg) > 0){
                failurefun(unlist(list("diffdf comparison has failed:\n",xmsg)))
                self$diff_result$result <- "Failed"
            } else {
                self$diff_result$result <- "Passed"
            }
            
            return(self$diff_result)
        }
    )
)














