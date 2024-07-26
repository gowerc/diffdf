# Column order checks work as expected

    Code
      diffdf(x1, x2, suppress_warnings = TRUE, check_column_order = TRUE)
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are differences in the column ordering between BASE and COMPARE !!
      All rows are shown in table below
      
        ===================================
         COLUMN  BASE-INDEX  COMPARE-INDEX 
        -----------------------------------
           z         3             4       
           q         4             3       
        -----------------------------------
      

---

    Code
      diffdf(x1, x2, suppress_warnings = TRUE, check_column_order = TRUE)
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are differences in the column ordering between BASE and COMPARE !!
      All rows are shown in table below
      
        ===================================
         COLUMN  BASE-INDEX  COMPARE-INDEX 
        -----------------------------------
           z         3             2       
           q         4             3       
        -----------------------------------
      
      There are columns in BASE that are not in COMPARE !!
      All rows are shown in table below
      
        =========
         COLUMNS 
        ---------
            y    
        ---------
      

---

    Code
      diffdf(x1, x2, suppress_warnings = TRUE, check_column_order = TRUE)
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE that are not in COMPARE !!
      All rows are shown in table below
      
        =========
         COLUMNS 
        ---------
           y1    
        ---------
      
      There are columns in COMPARE that are not in BASE !!
      All rows are shown in table below
      
        =========
         COLUMNS 
        ---------
           y2    
        ---------
      

# Column order checks work with null columns

    Code
      diffdf(d1, d2, suppress_warnings = TRUE, check_column_order = TRUE)
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are differences in the column ordering between BASE and COMPARE !!
      All rows are shown in table below
      
        ===================================
         COLUMN  BASE-INDEX  COMPARE-INDEX 
        -----------------------------------
           y         2             3       
           z         3             2       
        -----------------------------------
      

# By default column orders are not checked

    Code
      diffdf(d1, d2, suppress_warnings = TRUE)
    Output
      No issues were found!

