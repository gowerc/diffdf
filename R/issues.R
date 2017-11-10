



 
#' Issue Object
#' 
#' Base class for containing issues
#' These are used to control the flagging and warning messages associated with 
#' issues found in the rcompare function
#' 
#' @section Usage: Not intended for direct use. This object is intended to be inhertied by other 
#' objects which call it in an apprioate manner. 
#'
#' @section Arguments:
#' \code{value} - The values to be used by \code{checkfun} when deciding if an issue has been found or not.
#' This is also what is printed out when the print method for RCOMPARE is called
#' 
#' \code{message} - Warning message to be displayed if an issue is found
#' 
#' \code{checkfun} - Function to be run on \code{value} which decides whether or not an issue has been 
#' found
#'
#' \code{order} - The order in which the warning messages should be displayed in relation to the other
#' issues found
#' 
#' @section Methods:
#' \code{$get_print_message()} - Returns text for printing.
#'   
#' \code{$has_issue()}  - Returns T/F based upon whether \code{checkfun} finds an issue in \code{value}.
#'
#' \code{$get_issue_message()} - Returns the \code{message} if there are any issues.
#'   
#' \code{$get_text(dsin , row_limit = 20)} - Primary formating function used to prepare \code{dsin} prior
#' to printing. This is intended to be called via the inherited objects rather than directly.
#' 
#' @importFrom R6 R6Class
#' @import dplyr
#' @name issue
NULL


issue <- R6Class(
    "issue",
    public = list(
        
        value = NULL,
        has_issue = function() T,
        message = NULL,
        order = NULL,
        time = NULL,
        
        initialize = function( 
            value = NULL , 
            message = "" , 
            order = 999,
            time = NULL
        ){
            self$value = value
            self$message = message
            self$order = order
            self$time = time
        },
        
        get_print_message = function(){
            if( self$has_issue() ){  
                self$get_text()
            } else{
                NULL
            }
        },
        
        
        get_issue_message = function(){
            if ( self$has_issue() ) {
                return( self$message)
            } else {
                return("")
            }
        },
        
        
        get_text = function(dsin , row_limit = 10){
            
            if( nrow(dsin) == 0 ) {
                return("")
            }
            
            display_table <- dsin %>% 
                filter( row_number() < (row_limit + 1) )
            
            if ( nrow(dsin) > row_limit ){
                
                add_message <- paste0(
                    'First ',
                    row_limit,
                    ' rows are shown in table below'
                )
                
            } else {
                add_message <- 'All rows are shown in table below'
            } 
            
            display_table[]  <- apply(display_table, c(1, 2), crop_char_value)
            
            
            #paste together the message, the additional message, the table
            #and an extra final line
            
            TABLE <- mod_stargazer(
                display_table,
                type = 'text',
                summary = FALSE
            )
            
            RETURN <- paste(
                c(
                    self$message,
                    add_message,
                    TABLE,
                    '\n'
                ),
                collapse = '\n'
            )
            
            return(RETURN)
        }
    )
)


#' Issue Basic Object
#' 
#' Basic issue - Inherits from Issue Object 
#' These are used to control the flagging and warning messages associated with 
#' issues found in the rcompare function
#' 
#' @section Usage:  \code{ ob = issue_basic$new(...) }
#'
#' @section Methods:
#' \code{$get_text()} - Gets formated text suitable for printing
#' \code{$has_issue()} - Returns T/F depending on whether the issue is valid
#'
#' @name issue_basic
NULL
issue_basic <- R6Class(
    "issue_basic",
    inherit = issue,
    public = list(
        has_issue = function(){
            nrow(self$value) > 0
        },
        get_text = function(){
            super$get_text(self$value)
        }
    )
)

#' Issue List Object
#' 
#' List issue - Inherits from Issue Object 
#' This issue is meant to contain a list of other issues
#' These are used to control the flagging and warning messages associated with 
#' issues found in the rcompare function
#' 
#' @section Usage:  \code{ ob = issue_list$new(...) }
#'
#' @section Methods:
#' \code{$get_text()} - Gets formated text suitable for printing
#' \code{$has_issue()} - Returns T/F depending on whether the issue is valid
#'
#' @importFrom purrr map
#' @importFrom purrr map_lgl
#' @name issue_list
NULL
issue_list <- R6Class(
    "issue_list",
    inherit = issue,
    public = list(
        get_text = function(){
            map( self$value , function(x) x$get_text()) 
        },
        has_issue = function(){
            num_issues =  map_lgl( self$value , function(x) x$has_issue() ) %>% any
            return( num_issues ) 
        }
    ) 
)

#' Issue Vector Object
#' 
#' Vector issue - Inherits from Issue Object 
#' These are used to control the flagging and warning messages associated with 
#' issues found in the rcompare function
#' 
#' @section Usage:  \code{ ob = issue_vector$new(...) }
#'
#' @section Methods:
#' \code{$get_text()} - Gets formated text suitable for printing
#' \code{$has_issue()} - Returns T/F depending on whether the issue is valid
#'
#' @import dplyr
#' @importFrom tibble rownames_to_column
#' @name issue_vector
NULL
issue_vector <- R6Class(
    "issue_vector",
    inherit = issue,
    public = list(
        get_text = function(){
            
            datin_tibble <- self$value %>% 
                as.data.frame() %>% 
                rownames_to_column()
            
            names(datin_tibble) <- c('Variable', 'No of Differences')
            
            datin_tibble <- datin_tibble %>% 
                filter(`No of Differences` > 0)
            
            super$get_text(datin_tibble)
        },
        
        has_issue = function(){
            sum(self$value) > 0
        }
    ) 
)


