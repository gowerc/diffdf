
#' Print nice formating of the rcompare object
#' @param x comparison object created by rcompare()
#' @param VARIABLE specific variable to inspect the differences of
#' @param ... Additional arguments (not used)
#' @examples
#' # x <- iris[150,1]
#' # COMPARE <- rcompare( iris, x)
#' # print( COMPARE )
#' # print( COMPARE , "Sepal.Length" )
#' @export 
print.rcompare <- function(x, VARIABLE = NULL, ...){
    
    COMPARE <- x
    
    if ( !COMPARE$Issues){
        cat("No issues were found!")
        
    } else if ( !is.null(VARIABLE)) {
        
        outob <- make_textout( 
            datin     = COMPARE$VarDiffs[[VARIABLE]],
            row_limit = 100
        )
        
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
        getorder <- map_dbl(COMPARE, attr, 'order') %>% order
        COMPARE <- COMPARE[getorder]
        
        end_text <- map(COMPARE, make_textout) %>% 
            unlist() %>% 
            paste(collapse = '')
        
        outtext <- paste0(start_text, end_text)
        cat(outtext)
    }
    
    invisible(COMPARE)
}




#' mod_stargazer
#'
#' simple modification to return only the object, not print as well!
#' @importFrom stargazer stargazer 
#' @importFrom  utils capture.output
#' @param ... Any arguments to give to stargazer
mod_stargazer <- function(...){
    paste0(
        "  " , 
        capture.output(stargazer(..., rownames = F))
    )
}




#' crop_char_value
#'
#' Makes any character string above x chars
#' Reduce down to a x char string with ...
#' @param inval a single element value
#' @param crop_at character limit
crop_char_value <- function(inval, crop_at = 30 ){

    if ( is.null(inval) ){
        
        inval <- "" 
        
    } else if ( is.na(inval)){
        
        inval <- ""
        
    } else {
        
        inval <- as.character(inval)
        
    }
    
    charlength <- stringr::str_length(inval)
    
    if (charlength > crop_at ){
        
        outval <- substr(inval, 1, crop_at )
        outval <- paste0(outval, '...')
        
    } else {
        
        outval <- inval
        
    }
    
    outval
}


#'make_paste_object
#'
#' Pastes together the message and the data frame.
#' If more than 20 rows, its truncated
#' If this is an attribute message, switches
#' the data frame into the truncated tibble version to aid reading
#' @import dplyr
#' @param dataframe_in data frame to display
#' @param message Message which appears above data frame
#' @param row_limit This is the cut off point. Set at 20, but could be adjusted
make_pasteobject <- function(
    dataframe_in,
    message,
    row_limit = 20
){
    
    display_table <- dataframe_in %>% 
        filter( row_number() < (row_limit + 1) )
    
    if ( nrow(dataframe_in) > row_limit ){
        
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
    
    paste(
        c(
            message,
            add_message,
            TABLE,
            '\n'
        ),
        collapse = '\n'
    )
}





#'make_text out
#'
#' Performs check as to whether the comparison is empty or not
#' If empty, returns null
#' if not, returns the data frame pasted with the message
#' If att_expand is true, will return breakdown of attributes as well
#' @import dplyr
#' @param datin the data frame being tabulated
#' @param ... additional arguments to pass through
#' 
#' 
#' 
make_textout <- function(datin, ...){

    checkfun <- attr(datin, 'checkfun')
    
    if ( is.null(checkfun)){
        checkfun <- function(x) T
    }
    
    if( checkfun(datin) ){  
        UseMethod('make_textout')
    }
    else{
        NULL
    }
    
}



#'@export
make_textout.rcompare_basic <- function(datin, ...){
    make_pasteobject(
        datin,
        attr(datin, 'message'),
        ...
    )
}



#'@export
make_textout.rcompare_attrib <- function(datin, ...){

    TYPE <- attr(datin, 'type')
    
    if ( length(datin$VALUES.BASE) != 1 | length(datin$VALUES.COMP) != 1) {
        stop( "Unexpected number of values") 
    }
    
    BASE_VAL <- datin$VALUES.BASE[[1]]
    COMP_VAL <- datin$VALUES.COMP[[1]]
    browser()
    if ( 
        ( !is.character(BASE_VAL) & !is.null(BASE_VAL)) | 
        ( !is.character(COMP_VAL) & !is.null(COMP_VAL))
    ){
        stop( "Unsupported attribute type") 
    }
    
    ALL <- unique(c( 
        BASE_VAL ,
        COMP_VAL
    ))
    
    display <- data_frame(
        KEY = ALL
    ) %>% 
        left_join( data_frame(BASE = BASE_VAL, KEY = BASE_VAL) , by = "KEY") %>% 
        left_join( data_frame(COMP = COMP_VAL, KEY = COMP_VAL) , by = "KEY") %>% 
        select(-KEY)
    
    compare_out <- make_pasteobject(
        dataframe_in = display , 
        message = str_c('A breakdown of ', TYPE),
        ...
    )
    
    paste( compare_out, collapse ='\n')
}


#'@importFrom tibble rownames_to_column
#'@export  
make_textout.rcompare_vector <-  function(datin, ...){

    datin_tibble <- datin %>% 
        as.data.frame() %>% 
        rownames_to_column()
    
    names(datin_tibble) <- c('Variable', 'No of Differences')
    
    datin_tibble <- datin_tibble %>% 
        filter(`No of Differences` > 0)
    
    make_pasteobject(
        datin_tibble,
        attr(datin,'message'),
        ...
    )

}



#'@export
make_textout.rcompare_list   <-  function(datin){
    nonempty_list <- nonempty_list(datin)
    map(nonempty_list, make_textout)
}



