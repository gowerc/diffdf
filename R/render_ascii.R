




render_ascii <- function (object, ...) {
    UseMethod("render_ascii", object)
}



render_ascii.issue <- function(object, rowlimit = 10){
    top <- get_issue_message(object)
    
    dat <- get_issue_value(object)
    
    if( nrow(dat) > rowlimit){
        dat2 <- dat[seq_len(rowlimit),]
        caption <- paste0( "\nShowing " , rowlimit, " of ", nrow(dat), " observations")
    } else {
        dat2 <- dat
        caption <- ""
    }
    
    tab <- render_ascii(dat2)
    paste0(
        top,
        tab,
        caption,
        "\n\n"
    )
}



#' render_ascii
#' 
#' This function takes a data.frame and attempts to convert it into
#' a simple ascii format suitable for printing to the screen
#' It is assumed all variable values have a as.character() method
#' in order to cast them to character. 
#' @param dat Input dataset to convert into a ascii table
#' @param line_prefix Symbols to prefix infront of every line of the table
render_ascii.data.frame <- function(dat, line_prefix = "  "){
    ## Convert every value to character and crop to a suitable length
    dat_chr  <- apply(dat, c(1, 2), as_cropped_char)
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









