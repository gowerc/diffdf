# can handle null datasets

    Code
      res
    Output
      Differences found between the objects!
      
      Summary of BASE and COMPARE
        ======================================
          PROPERTY       BASE         COMP    
        --------------------------------------
            Name     data.frame()     iris    
           Class      data.frame   data.frame 
          Rows(#)         0           150     
         Columns(#)       0            5      
        --------------------------------------
      
      
      There are rows in COMPARE that are not in BASE
      First 10 of 150 rows are shown in table below
        ===============
         ..ROWNUMBER.. 
        ---------------
               1       
               2       
               3       
               4       
               5       
               6       
               7       
               8       
               9       
              10       
        ---------------
      
      
      There are columns in COMPARE that are not in BASE
        ==============
           COLUMNS    
        --------------
         Sepal.Length 
         Sepal.Width  
         Petal.Length 
         Petal.Width  
           Species    
        --------------
      
      

---

    Code
      res
    Output
      Differences found between the objects!
      
      Summary of BASE and COMPARE
        ======================================
          PROPERTY      BASE         COMP     
        --------------------------------------
            Name        iris     data.frame() 
           Class     data.frame   data.frame  
          Rows(#)       150           0       
         Columns(#)      5            0       
        --------------------------------------
      
      
      There are rows in BASE that are not in COMPARE
      First 10 of 150 rows are shown in table below
        ===============
         ..ROWNUMBER.. 
        ---------------
               1       
               2       
               3       
               4       
               5       
               6       
               7       
               8       
               9       
              10       
        ---------------
      
      
      There are columns in BASE that are not in COMPARE
        ==============
           COLUMNS    
        --------------
         Sepal.Length 
         Sepal.Width  
         Petal.Length 
         Petal.Width  
           Species    
        --------------
      
      

---

    Code
      res
    Output
      Differences found between the objects!
      
      Summary of BASE and COMPARE
        ==================================================================
          PROPERTY             BASE                       COMP            
        ------------------------------------------------------------------
            Name                x1                  "x1[FALSE, "x"]"      
           Class     "tbl_df, tbl, data.frame"  "tbl_df, tbl, data.frame" 
          Rows(#)                3                          0             
         Columns(#)              2                          1             
        ------------------------------------------------------------------
      
      
      There are rows in BASE that are not in COMPARE
        ===============
         ..ROWNUMBER.. 
        ---------------
               1       
               2       
               3       
        ---------------
      
      
      There are columns in BASE that are not in COMPARE
        =========
         COLUMNS 
        ---------
            y    
        ---------
      
      

---

    Code
      res
    Output
      Differences found between the objects!
      
      Summary of BASE and COMPARE
        ==================================================================
          PROPERTY             BASE                       COMP            
        ------------------------------------------------------------------
            Name         "x1[FALSE, "x"]"                  x1             
           Class     "tbl_df, tbl, data.frame"  "tbl_df, tbl, data.frame" 
          Rows(#)                0                          3             
         Columns(#)              1                          2             
        ------------------------------------------------------------------
      
      
      There are rows in COMPARE that are not in BASE
        ===============
         ..ROWNUMBER.. 
        ---------------
               1       
               2       
               3       
        ---------------
      
      
      There are columns in COMPARE that are not in BASE
        =========
         COLUMNS 
        ---------
            y    
        ---------
      
      

# can handle non-overlapping keys

    Code
      res
    Output
      Differences found between the objects!
      
      Summary of BASE and COMPARE
        ====================================
          PROPERTY      BASE        COMP    
        ------------------------------------
            Name         x1          x2     
           Class     data.frame  data.frame 
          Rows(#)        2           2      
         Columns(#)      2           2      
        ------------------------------------
      
      
      There are rows in BASE that are not in COMPARE
        ====
         ID 
        ----
         A  
        ----
      
      
      There are rows in COMPARE that are not in BASE
        ====
         ID 
        ----
         B  
        ----
      
      

