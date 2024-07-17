# Column order checks work as expected

    Code
      diffdf(x1, x2, suppress_warnings = TRUE)
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
      diffdf(x1, x2, suppress_warnings = TRUE)
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
      diffdf(x1, x2, suppress_warnings = TRUE)
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
      

