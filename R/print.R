
#' Print rcompare objects
#' 
#' Print nicely formated version of an rcompare object
#' @param x comparison object created by rcompare().
#' @param VARIABLE specific variable to inspect the differences of (string).
#' @param ... Additional arguments (not used)
#' @examples
#' x <- subset( iris , -Species )
#' x[1,2] <- 5
#' COMPARE <- rcompare( iris, x)
#' print( COMPARE )
#' print( COMPARE , "Sepal.Length" )
#' @importFrom purrr map_dbl
#' @importFrom purrr map
#' @export 
print.rcompare <- function(x, VARIABLE = NULL, ...){
    
    COMPARE <- x
    
    if ( !COMPARE$Issues){
        cat("No issues were found!")
        
    } else if ( !is.null(VARIABLE)) {
        
        outob <- get_print_message(COMPARE$VarDiffs$value[[VARIABLE]])
        
        if(is.null(outob)){
            cat('Variable matched')
        } else {
            cat(outob)
        }
        
    } else {
        
        start_text <- paste0(
            'Differences found between the objects!\n\n',
            'A summary is given below.\n\n',
            'Please use print(, Variable = "Name") to examine in more, ',
            'detail where necessary.\n\n'
        )
        
        #Start by looking at simple comparisons
        #extra columns/rows and illegal columns
        #We make a set of 7 arguments to pass to pastefun, defined above
        COMPARE$Issues <- NULL
        getorder <- map_dbl(COMPARE, function(x) x$order) %>% order
        COMPARE <- COMPARE[getorder]
        
        end_text <- map(COMPARE, function(x) get_print_message(x) ) %>% 
            unlist() %>% 
            paste(collapse = '')
        
        outtext <- paste0(start_text, end_text)
        cat(outtext)
    }
    
    invisible(COMPARE)
}

#' as_ascii_table
#' 
#' This function takes a data.frame and attempts to convert it into
#' a simple ascii format suitable for printing to the screen
#' It is assumed all variable values have a as.character() method
#' in order to cast them to character. 
#' @param dat Input dataset to convert into a ascii table
#' @param line_prefix Symbols to prefix infront of every line of the table
#' @importFrom purrr reduce
#' @importFrom purrr transpose
#' @importFrom stringr str_pad
#' @importFrom stringr str_length
as_ascii_table <- function(dat, line_prefix = "  "){
    
    ## Convert every value to character and crop to a suitable length
    dat[]  <- apply(dat, c(1, 2), as_cropped_char)
    hold <- list()
    COLS <- colnames(dat)
    
    ### For each column extract core elements (width, values , title) and pad out
    ### each string to be a suitable length
    for ( i in 1:ncol(dat)){
        COL <- COLS[i]
        VALUES <- dat[[i]]

        JOINT <- c(COL , VALUES)
        WIDTH <- max( str_length(JOINT)) + 2
        
        hold[[COL]] <- list() 
        hold[[COL]]$WIDTH <- WIDTH 
        hold[[COL]]$VALUES <- str_pad(VALUES , width = WIDTH , side = "both")  
        hold[[COL]]$HEADER <- str_pad(COL    , width = WIDTH , side = "both")
    }
    
    ### Collapse into a single value per component ( title , values, width )
    thold  <- transpose(hold)
    tvals  <- reduce( thold$VALUES , paste0 ) 
    thead  <- reduce( thold$HEADER , paste0)
    twidth <- reduce( thold$WIDTH , sum)
    
    ### Create header and footer lines
    TLINE <- paste0(rep("=" , twidth), collapse = "")
    LINE  <- paste0(rep("-" , twidth), collapse = "")
    FVALS <- paste0(line_prefix, tvals , collapse = "\n")
    
    ### Output table
    paste0( 
        "\n",
        line_prefix, TLINE, "\n",
        line_prefix, thead, "\n",
        line_prefix, LINE,  "\n",
        FVALS, "\n",
        line_prefix, LINE
    )     
}



#' as_cropped_char
#'
#' Makes any character string above x chars
#' Reduce down to a x char string with ...
#' @param inval a single element value
#' @param crop_at character limit
#' @importFrom stringr str_length
as_cropped_char <- function(inval, crop_at = 30 ){

    if ( is.null(inval) ){
        
        inval <- "<NULL>" 
        
    } else if ( is.na(inval)){
        
        inval <- "<NA>"
        
    } else {
        
        inval <- as.character(inval)
        
    }
    
    charlength <- str_length(inval)
    
    if (charlength > crop_at ){
        
        outval <- substr(inval, 1, crop_at )
        outval <- paste0(outval, '...')
        
    } else {
        
        outval <- inval
        
    }
    
    outval
}



