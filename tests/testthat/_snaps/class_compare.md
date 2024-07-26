# Class comparison works as expected when enabled

    Code
      diffdf(iris, iris, check_df_class = TRUE)
    Output
      No issues were found!

---

    Code
      diffdf(tibble(iris), iris, suppress_warnings = TRUE, check_df_class = TRUE)
    Output
      Differences found between the objects!
      
      Summary of BASE and COMPARE
        ===================================================
          PROPERTY             BASE                COMP    
        ---------------------------------------------------
            Name           tibble(iris)            iris    
           Class     "tbl_df, tbl, data.frame"  data.frame 
          Rows(#)               150                150     
         Columns(#)              5                  5      
        ---------------------------------------------------
      
      

