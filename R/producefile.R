


#' mod_stargazer
#'
#' simple modification to return only the object, not print as well!
#' @importFrom stargazer stargazer 
#' @importFrom  utils capture.output
#' @param ... Any arguments to give to stargazer
mod_stargazer <- function(...){
  capture.output(stargazer(...))
}




#'valuefixer
#'
#' Makes any character string above 20 chars
#' Reduce down to a 20 char string with ...
#' @param inval a single element value
valuefixer <- function(inval){
  
  inval <- as.character(inval)
  charlength <- stringr::str_length(inval)
  
  if (charlength > 20){
    
    outval <- substr(inval, 1, 20)
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
#' @param att_expand T/F whether we should use the trunc_mat argument
#' @param row_limit This is the cut off point. Set at 20, but could be adjusted
make_pasteobject <- function(
  dataframe_in,
  message,
  att_expand,
  row_limit = 20
){
  
  if (nrow(dataframe_in) > row_limit){
    
    display_table <- dataframe_in %>% 
      filter(row_number() < (row_limit + 1))
    
    add_message <- paste0(
      '  (First ',
      row_limit,
      ' rows are shown in table below)'
    )
    
  } else {
    display_table <- dataframe_in
    add_message <- '  (All rows are shown in table below)'
  }
  
  if (att_expand){
    display_table <- trunc_mat(display_table)$table
  } else {
    display_table[]  <- apply(display_table, c(1, 2), valuefixer)
  }
  
  #paste together the message, the additional message, the table
  #and an extra final line
  
  paste(
    c(
      message,
      add_message,
      mod_stargazer(
        display_table,
        type = 'text',
        summary = FALSE
      ),
      '\n'
    ),
    collapse = '\n'
  )
}

#' attribute_breakdown
#' 
#' Split up an attribute data frame to just get base or compare
#' This is a convenience function to tidy up code
#' @import dplyr
#' @param attkeep att we're going to keep. Should be a char vector, either BASEatt or COMPatt
#' @param attdrop att we're going to drop. Should be a char vector, either BASEatt or COMPatt
#' @param datin The data frame being reshaped
attribute_breakdown <- function(attkeep, attdrop, datin){
  
  compare_ob <- datin %>% 
    select(-starts_with(attdrop)) %>% 
    filter_(paste0(attkeep," != 'NULL'"))
  
  if (nrow(compare_ob)>0){
    compare_ob <- compare_ob %>% tidyr::unnest_(attkeep) %>% 
      make_pasteobject('A breakdown of Base attributes', FALSE)
    
  } else {
    compare_ob <- NULL
  }
  compare_ob
}



#'make_text out
#'
#' Performs check as to whether the comparison is empty or not
#' If empty, returns null
#' if not, returns the data frame pasted with the message
#' If att_expand is true, will return breakdown of attributes as well
#' @import dplyr
#' @param fcompare The function used to check whether to display the item or not
#' @param message The message displayed above the column
#' @param datin the data frame being tabulated
#' @param att_expand false by default, whether we should have additional attribute breakdown
#' 
#' 
#' 
make_textout <- function(datin){
  UseMethod('make_textout')
}
make_textout.rcompare_basic <- function(datin){
  

  if(  attr(datin, 'checkfun')(datin) ){
    
    out <- make_pasteobject(
      datin,
      attr(datin, 'message'),
      att_expand = FALSE)
    out
    }else{
      NULL
    }
  }
make_textout.rcompare_attrib <- function(datin){
    
  
    if(  attr(datin, 'checkfun')(datin)  ){
      
      out <- make_pasteobject(
        datin,
        attr(datin, 'message'),
        att_expand = TRUE)
      
      base_compare <- attribute_breakdown( 'VALUES.BASE', 'VALUES.COMP', datin) 
      comp_compare <- attribute_breakdown('VALUES.COMP', 'VALUES.BASE', datin) 
      
      out <- paste(out, base_compare, comp_compare, collapse ='\n')
      out
      }else{
        NULL
      }
    }
    
make_textout.rcompare_vector <-  function(datin){
  
 
  if(  attr(datin, 'checkfun')(datin) ){  
    
    datin_tibble <- datin %>% 
      as.data.frame() %>% 
      rownames_to_column()
    
    names(datin_tibble) <- c('Variable', 'No of Differences')
    
    datin_tibble <- datin_tibble %>% 
      filter(`No of Differences` > 0)
    
   make_pasteobject(
      datin_tibble,
      attr(datin,'message'),
      att_expand = FALSE
    )
  }else{
    NULL
  }
}

make_textout.rcompare_list   <-  function(datin){
  if( attr(datin, 'checkfun')(datin) ){  
    nonempty_list <- nonempty_list(datin)
    map(nonempty_list, make_textout)
  }
  else{
    NULL
  }
}
    

  
  
  #'Produce_file
  #'
  #'Outputs a text file based on the compare object
  #'@importFrom purrr pmap
  #'@import dplyr
  #'@importFrom tibble rownames_to_column
  #'@param outfile location to save file
  #'@param COMPARE the compare object
  produce_file <- function(outfile, COMPARE){
    if(!COMPARE[["Issues"]]){
      
      outtext <- 'Objects are identical'
      
    } else {
      COMPARE$Issues <- NULL
      
      start_text <- paste0(
        'Differences found between the objects!\n\n',
        'A summary is given below.\n\n',
        'Please use print() to examine in more, ',
        'detail where necessary.\n\n'
      )
      
      #Start by looking at simple comparisons
      #extra columns/rows and illegal columns
      #We make a set of 7 arguments to pass to pastefun, defined above
      
      getorder <- map_dbl(COMPARE, attr, 'order')
      COMPARE <- COMPARE[getorder]
      
      end_text <- map(COMPARE, make_textout) %>% 
        unlist() %>% 
        paste(collapse = '')
    outtext <- paste0(start_text, end_text)
      
    }
    write(outtext, file = outfile)
  }
  