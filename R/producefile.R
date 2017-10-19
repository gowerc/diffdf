mod_stargazer <- function(...){
  capture.output(stargazer(...))
}


make_pasteobject <- function(dataframe_in,
                             message,
                             att_expand){
  if (nrow(dataframe_in) > 20)
  {
    display_table <- dataframe_in %>% 
      filter(row_number()<21)
    add_message <- '  :Displayed below, the first 20 rows are shown'
  }else{
    display_table <- dataframe_in
    add_message <- '  :Displayed below, all rows are shown'
  }
  if (att_expand){
    display_table<- trunc_mat(display_table)$table
  }
  
  paste(c(message,
                 add_message,
                 mod_stargazer(display_table,
                               type = 'text',
                               summary = FALSE), '\n'),
               collapse = '\n')
}

pastefun <- function(fcompare, message, comparg, att_expand = FALSE){
  fcompare <- get(fcompare)
  if( fcompare (COMPARE[[comparg]]) ){
    
    out <- make_pasteobject(COMPARE[[comparg]],
                            message, att_expand)
    if(att_expand){
      base_compare <- COMPARE[[comparg]] %>% 
        select(-COMPatt) %>% 
        filter(BASEatt!='NULL')
      if (nrow(base_compare)>0){
        base_compare <- base_compare %>% tidyr::unnest(BASEatt) %>% 
        make_pasteobject('A breakdown of Base attributes', FALSE)
      }else{
        base_compare <- NULL
      }
      comp_compare <- COMPARE[[comparg]] %>% 
        select(-BASEatt) %>% 
        filter(COMPatt!='NULL')
      if (nrow(comp_compare)>0){
        comp_compare <- comp_compare %>% tidyr::unnest(COMPatt) %>% 
          make_pasteobject('A breakdown of Compare attributes', FALSE)
      }else{
        comp_compare <- NULL
      }
      out <- paste(out, base_compare, comp_compare, collapse ='\n')
    }
    out
  }else{
    NULL
  }

}


produce_file <- function(outfile, COMPARE)
{
  if(!check_for_issues(COMPARE, TRUE))
  {
    outtext <- 'Objects are identical'
  }else{
    outtext <- 'Differences found between the objects!\n\nA summary is given below.\n\nPlease use print() to examine in more detail where necessary.\n\n'

    #Start by looking at simple comparisons, extra columns/rows and illegal columns
    
    argcol <- rep(c('nrow', 'ncol', 'nrow'), c(4, 2, 1))
    messages <- c('Extra Rows found in Base',
                  'Extra Rows found in Compare',
                  'Extra Columns found in Base',
                  'Extra Columns found in Compare',
                  'There are Columns in Base with unsupported modes',
                  'There are Columns in Compare with unsupported modes',
                  'There are Columns in Base and Compare with different modes')
    compare_args <- c("ExtRowsBase",
                      "ExtRowsComp",
                      "ExtColsBase",
                      "ExtColsComp",
                      "IllegalColsBase",
                      "IllegalColsCompare",
                      "VarModeDiffs"
                      )
    outtext2 <- pmap(list(argcol, messages, compare_args), pastefun) %>% 
      unlist() %>% 
      paste(collapse = '\n')
   
    outtext <- paste(c(outtext, outtext2), collapse = '\n')

    #Now look at attributes. These needs to be handled slightly differently, hence the extra argument
    argcol <- rep('nrow', 3)
    messages <- c("There are Factor Columns in BASE and COMPARE with different levels",
                  "There are Columns in BASE and COMPARE with different labels",
                  "There are columns in BASE and COMPARE with differing attributes")
    
    compare_args <- c("FactorlevelDiffs", "LabelDiffs", "AttribDiffs")
    
    outtext3 <- pmap(list(argcol, messages, compare_args), pastefun, att_expand = TRUE) %>% 
      unlist() %>% 
      paste(collapse = '\n')
    
    outtext <- paste(c(outtext, outtext3), collapse = '\n')
    
    #Finally we deal with the actual differences!
    
    outtext_numdiff <- pastefun(sum, 'The following columns in BASE and COMPARE were found to be different in the amount displayed', "NumDiff")
    
    
  }
  write(outtext, file = outfile)
}