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
  }else{
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
make_pasteobject <- function(dataframe_in,
                             message,
                             att_expand,
                             row_limit = 20){
  
  if (nrow(dataframe_in) > row_limit)
  {
    display_table <- dataframe_in %>% 
      filter(row_number() < (row_limit + 1))
    
    add_message <- paste0('  (First ',
                          row_limit,
                          ' rows are shown in table below)')
  }else{
    display_table <- dataframe_in
    add_message <- '  (All rows are shown in table below)'
  }
  
  if (att_expand){
    display_table <- trunc_mat(display_table)$table
  }else{
    display_table[]  <- apply(display_table, c(1, 2), valuefixer)
  }
  
  #paste together the message, the additional message, the table
  #and an extra final line
  
  paste(c(message,
          add_message,
          mod_stargazer(display_table,
                        type = 'text',
                        summary = FALSE),
          '\n'),
        collapse = '\n')
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
    
  }else{
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
make_textout <- function(fcompare, message, datin, att_expand = FALSE){
  fcompare <- get(fcompare)
  if( fcompare (datin) ){
    
    out <- make_pasteobject(datin,
                            message,
                            att_expand)
    if(att_expand){
      base_compare <- attribute_breakdown( 'BASEatt', 'COMPatt', datin) 
      comp_compare <- attribute_breakdown('COMPatt', 'BASEatt', datin) 
      out <- paste(out, base_compare, comp_compare, collapse ='\n')
    }
    out
  }else{
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
produce_file <- function(outfile, COMPARE)
{
  if(!check_for_issues(COMPARE, TRUE))
  {
    outtext <- 'Objects are identical'
  }else{
    start_text <- paste0('Differences found between the objects!\n\n',
                         'A summary is given below.\n\n',
                         'Please use print() to examine in more,',
                         'detail where necessary.\n\n')
    
    #Start by looking at simple comparisons
    #extra columns/rows and illegal columns
    #We make a set of 7 arguments to pass to pastefun, defined above
    
    argcol <- rep(c('nrow', 'ncol', 'nrow'), c(4, 2, 1))
    messages <- c('Extra Rows found in Base','Extra Rows found in Compare',
                  'Extra Columns found in Base', 'Extra Columns found in Compare',
                  'There are Columns in Base with unsupported modes',
                  'There are Columns in Compare with unsupported modes',
                  'There are Columns in Base and Compare with different modes')
    datin <- COMPARE[c("ExtRowsBase", "ExtRowsComp", "ExtColsBase",
                       "ExtColsComp", "IllegalColsBase", "IllegalColsCompare",
                       "VarModeDiffs")]
    
    row_columns_text <- pmap(list(argcol, messages, datin),
                             make_textout) %>% 
      unlist() %>% 
      paste(collapse = '\n')
    
    
    #Now look at attributes. These needs to be handled slightly
    #differently, hence the extra argument
    argcol <- rep('nrow', 3)
    messages <- c("There are Factor Columns in BASE and COMPARE with different levels",
                  "There are Columns in BASE and COMPARE with different labels",
                  "There are columns in BASE and COMPARE with differing attributes")
    
    datin <- COMPARE[c("FactorlevelDiffs", "LabelDiffs", "AttribDiffs")]
    
    attrib_text <- pmap(list(argcol, messages, datin ),
                        make_textout,
                        att_expand = TRUE) %>% 
      unlist() %>% 
      paste(collapse = '\n')
    
    
    #Finally we deal with the actual differences!
    if( sum(COMPARE[["NumDiff"]])){  
      
      numdiff_tibble <- COMPARE[["NumDiff"]] %>% 
        as.data.frame() %>% 
        rownames_to_column()
      
      names(numdiff_tibble) <- c('Variable', 'No of Differences')
      numdiff_tibble <- numdiff_tibble %>% 
        filter(`No of Differences` > 0)
      
      numdiff_text <- make_pasteobject(numdiff_tibble,
                                       paste0('These columns in BASE/COMPARE ',
                                              'had the following number of ',
                                              'differences'),
                                       att_expand = FALSE)   
      
      diffbreakdown_text <- 'The Variables with differences are tabulated below'
      
      nonempty_compare <- COMPARE[["VarDiffs"]]
      noenmpty_compare <- nonempty_compare[map(nonempty_compare, nrow)>0]
      all_diffs_text <- pmap(list(noenmpty_compare,
                                  paste0('Variable = ',
                                         names(noenmpty_compare))),
                             make_pasteobject,
                             att_expand = FALSE) %>% 
        unlist() %>% 
        paste(collapse = '\n')
      
      breakdownfull_text <- paste(c(numdiff_text,
                                    diffbreakdown_text,
                                    all_diffs_text),
                                  collapse = '\n\n')
    }
    outtext <- paste(c(start_text,
                       row_columns_text,
                       attrib_text,
                       breakdownfull_text),
                     collapse = '\n')
    
  }
  write(outtext, file = outfile)
}