#' string_pad
#'  
#' Utility function used to replicate str_pad. Adds white space to either end 
#' of a string to get it to equal the desired length
#' @param x string
#' @param width desired length 
string_pad <- function(x , width){
    if ( nchar(x) >= width) return(x)
    width <- width - nchar(x)
    left <- paste0( rep ( " " ,  floor( width/2) )  , collapse= "") 
    right <- paste0( rep ( " " ,  ceiling( width/2) )  , collapse= "") 
    paste0(  left , x ,right , collapse = "")
}


#' recursive_reduce
#'  
#' Utility function used to replicated purrr::reduce. Recursively applies a 
#' function to a list of elements until only 1 element remains
#' @param .l list of values to apply a function to
#' @param .f function to apply to each each element of the list in turn
#' i.e. .l[[1]] <- .f( .l[[1]] , .l[[2]]) ; .l[[1]] <- .f( .l[[1]] , .l[[3]])
recursive_reduce <- function(.l , .f){
    if (length(.l) != 1){
        .l[[2]] <- .f( .l[[1]] , .l[[2]])  
        return( recursive_reduce( .l[-1] , .f))
    } else {
        return(.l[[1]])
    }
}

#' invert
#'
#' Utility function used to replicated purrr::transpose. Turns a list inside
#' out. 
#' @param x list 
invert <- function(x){
    x2 <- list() 
    cnames <- names(x)
    tnames <- names(x[[1]])
    for ( i in tnames ){
        x2[[i]] <- list()
        for (j in cnames){
            x2[[i]][[j]] <- x[[j]][[i]]
        }
    }
    return(x2)
}



render_ascii <- R6Class(
    "render_ascii", 
    inherit = render,
    public = list(

        #' file
        #' 
        #' Method for writing ascii object to a file
        #' @param filename file to write to 
        file = function(filename){
            sink(filename)
            cat(self$strings, sep = "\n")
            sink()
        },
        
        #' display
        #' 
        #' Method to print ascii object to the screen
        display = function(){
            cat(self$strings, sep = "\n")
        },
        
        #' h1
        #' 
        #' Convert string into ascii h1 format
        #' @param x string
        h1 = function(x) return(x),
        
        
        #' h2
        #' 
        #' Convert string into ascii h1 format
        #' @param x string
        h2 = function(x) return(x),
        
        
        #' h3
        #' 
        #' Convert string into ascii h1 format
        #' @param x string
        h3 = function(x) return(x),
        
        
        #' h4
        #' 
        #' Convert string into ascii h1 format
        #' @param x string
        h4 = function(x) return(x),
        
        
        #' p
        #' 
        #' Convert string into ascii h1 format
        #' @param x string
        p = function(x) return(x),
        
        
        #' br
        #' 
        #' Convert string into ascii h1 format
        #' @param x string
        br = function(x) return("\n"),
       
        
        #' table
        #' 
        #' This function takes a data.frame and attempts to convert it into
        #' a simple ascii format suitable for printing to the screen
        #' It is assumed all variable values have a as.character() method
        #' in order to cast them to character. 
        #' 
        #' @param df Input dataset to convert into a ascii table
        #' @param limitstring A string indicating whether the dataset has been subset
        table = function(df, limitstring){
            line_prefix = "  "
            
            ## Convert every value to character and crop to a suitable length
            dat  <- as.data.table(apply(df, c(1, 2), as_cropped_char))
            
            hold <- list()
            COLS <- colnames(dat)
            
            ### For each column extract core elements (width, values , title) and pad out
            ### each string to be a suitable length
            for ( i in 1:ncol(dat)){
                COL <- COLS[i]
                VALUES <- dat[[i]]
                
                JOINT <- c(COL , VALUES)
                WIDTH <- max( sapply(JOINT, nchar)) + 2
                
                hold[[COL]] <- list() 
                hold[[COL]]$WIDTH <- WIDTH 
                hold[[COL]]$VALUES <- sapply( VALUES ,string_pad,  width = WIDTH )  
                hold[[COL]]$HEADER <- sapply( COL ,string_pad,  width = WIDTH )
            }
            
            ### Collapse into a single value per component ( title , values, width )
            thold  <- invert(hold)
            tvals  <- recursive_reduce( thold$VALUES , paste0 ) 
            thead  <- recursive_reduce( thold$HEADER , paste0)
            twidth <- recursive_reduce( thold$WIDTH , sum)
            
            ### Create header and footer lines
            TLINE <- paste0(rep("=" , twidth), collapse = "")
            LINE  <- paste0(rep("-" , twidth), collapse = "")
            FVALS <- paste0(line_prefix, tvals , collapse = "\n")
            
            if( !is.na(limitstring)){
                caption <- paste0("\n",line_prefix, limitstring)
            } else {
                caption <- NULL
            }
            
            ### Output table
            paste0( 
                line_prefix, TLINE, "\n",
                line_prefix, thead, "\n",
                line_prefix, LINE,  "\n",
                FVALS, "\n",
                line_prefix, LINE,
                caption
            )     
        }
    )
)





