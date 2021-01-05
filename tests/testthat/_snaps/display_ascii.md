# Ascii - Identical

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            1                       1            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Passed 
          Values      Passed 
      -----------------------
    
    All checks have passed
    
    

---

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            1                       1            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Passed 
          Values      Passed 
      -----------------------
    
    All checks have passed
    
    

# Ascii - Different Values

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Passed 
          Values      Failed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Value Mismatches
    
    Variable: CONTINUOUS
      ==========================================
       ..ROWNUMBER..        Base        Compare 
      ------------------------------------------
             1        15.6572034240234     1    
             5        26.1530304291068     2    
             7        16.2928917544776     3    
      ------------------------------------------
    
    

---

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Passed 
          Values      Failed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Value Mismatches
    
    Variable: CONTINUOUS
      =======================================
       ..ROWNUMBER..  Base      Compare      
      ---------------------------------------
             1         1    15.6572034240234 
             5         2    26.1530304291068 
             7         3    16.2928917544776 
      ---------------------------------------
    
    

# Ascii - Different Attributes

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Failed 
          Values      Passed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Attribute Mismatches
    
      ==============================================================
       Variable  Attribute  Base               Compare              
      --------------------------------------------------------------
        BINARY   something  NULL  list(Sepal.Length = c(5.1, 4.9... 
      --------------------------------------------------------------
    

---

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Failed 
          Values      Passed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Attribute Mismatches
    
      =================================================================
       Variable  Attribute                Base                 Compare 
      -----------------------------------------------------------------
        BINARY   something  list(Sepal.Length = c(5.1, 4.9...   NULL   
      -----------------------------------------------------------------
    

# Ascii - Different Levels

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Failed 
          Values      Failed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Attribute Mismatches
    
      ============================================================
        Variable    Attribute        Base            Compare      
      ------------------------------------------------------------
       CATEGORICAL   levels    c("A", "B", "C")  c("A", "B", "D") 
      ------------------------------------------------------------
    
    Value Mismatches
    
    Variable: CATEGORICAL
      ==============================
       ..ROWNUMBER..  Base  Compare 
      ------------------------------
             1         C       D    
             2         C       D    
             3         C       D    
             6         C       D    
             7         C       D    
            15         C       D    
            17         C       D    
            20         C       D    
      ------------------------------
    
    

---

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Failed 
          Values      Failed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Attribute Mismatches
    
      ============================================================
        Variable    Attribute        Base            Compare      
      ------------------------------------------------------------
       CATEGORICAL   levels    c("A", "B", "D")  c("A", "B", "C") 
      ------------------------------------------------------------
    
    Value Mismatches
    
    Variable: CATEGORICAL
      ==============================
       ..ROWNUMBER..  Base  Compare 
      ------------------------------
             1         D       C    
             2         D       C    
             3         D       C    
             6         D       C    
             7         D       C    
            15         D       C    
            17         D       C    
            20         D       C    
      ------------------------------
    
    

# Ascii - Difference Class

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Failed 
        Attributes    Passed 
          Values      Passed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Class Mismatches
    
      ================================================
       Variable  Base             Compare             
      ------------------------------------------------
         DATE    Date  c("A_DATE", "b_date", "cDate") 
      ------------------------------------------------
    

---

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Failed 
        Attributes    Passed 
          Values      Passed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Class Mismatches
    
      ===================================================
       Variable               Base               Compare 
      ---------------------------------------------------
         DATE    c("A_DATE", "b_date", "cDate")   Date   
      ---------------------------------------------------
    

# Ascii - Different Modes

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Failed 
           Class      Passed 
        Attributes    Passed 
          Values      Passed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Mode Mismatches
    
      ================================
       VARIABLE  MODE.BASE  MODE.COMP 
      --------------------------------
       INTEGER    numeric   character 
      --------------------------------
    

---

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Failed 
           Class      Passed 
        Attributes    Passed 
          Values      Passed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Mode Mismatches
    
      ================================
       VARIABLE  MODE.BASE  MODE.COMP 
      --------------------------------
       INTEGER   character   numeric  
      --------------------------------
    

# Ascii - Missing Columns

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            3                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Failed 
       ExtraColsComp  Passed 
           Mode       Failed 
           Class      Passed 
        Attributes    Passed 
          Values      Passed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Extra Columns in Base
    
      =========
       Columns 
      ---------
       BINARY  
      ---------
    
    Mode Mismatches
    
      ================================
       VARIABLE  MODE.BASE  MODE.COMP 
      --------------------------------
       INTEGER    numeric   character 
      --------------------------------
    

---

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            3                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Failed 
       ExtraColsComp  Passed 
           Mode       Failed 
           Class      Passed 
        Attributes    Passed 
          Values      Passed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Extra Columns in Base
    
      =========
       Columns 
      ---------
       BINARY  
      ---------
    
    Mode Mismatches
    
      ================================
       VARIABLE  MODE.BASE  MODE.COMP 
      --------------------------------
       INTEGER   character   numeric  
      --------------------------------
    

# Ascii - Missing Rows

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      10           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Failed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Passed 
          Values      Passed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Extra Rows in Base
    
      ===============
       ..ROWNUMBER.. 
      ---------------
            11       
            12       
            13       
            14       
            15       
            16       
            17       
            18       
            19       
            20       
      ---------------
    

---

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      10           
       Number of columns            2                       2            
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Failed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Passed 
          Values      Passed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Extra Rows in Base
    
      ===============
       ..ROWNUMBER.. 
      ---------------
            11       
            12       
            13       
            14       
            15       
            16       
            17       
            18       
            19       
            20       
      ---------------
    

# Ascii - Everything

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            11                      11           
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Failed 
           Class      Failed 
        Attributes    Failed 
          Values      Failed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Mode Mismatches
    
      ================================
       VARIABLE  MODE.BASE  MODE.COMP 
      --------------------------------
       INTEGER    numeric   character 
      --------------------------------
    
    Class Mismatches
    
      ================================================
       Variable  Base             Compare             
      ------------------------------------------------
         DATE    Date  c("A_DATE", "b_date", "cDate") 
      ------------------------------------------------
    
    Attribute Mismatches
    
      =============================================================================
        Variable    Attribute        Base                     Compare              
      -----------------------------------------------------------------------------
         BINARY     something        NULL        list(Sepal.Length = c(5.1, 4.9... 
       CATEGORICAL   levels    c("A", "B", "C")          c("A", "B", "D")          
        DATETIME      label          NULL        This is the label for my amazi... 
      -----------------------------------------------------------------------------
    
    Value Mismatches
    
    Variable: GROUP2
      ==============================
       ..ROWNUMBER..  Base  Compare 
      ------------------------------
             1         1      47    
             2         2      52    
             3         3      53    
             4         4      38    
             5         5      52    
             6         6      52    
             7         7      54    
             8         8      48    
             9         9      52    
            10         10     46    
      ------------------------------
      Showing 10 of 20 observations
    
    Variable: CONTINUOUS
      ==========================================
       ..ROWNUMBER..        Base        Compare 
      ------------------------------------------
             1        15.6572034240234     1    
             5        26.1530304291068     2    
             7        16.2928917544776     3    
      ------------------------------------------
    
    Variable: CATEGORICAL
      ==============================
       ..ROWNUMBER..  Base  Compare 
      ------------------------------
             1         C       D    
             2         C       D    
             3         C       D    
             6         C       D    
             7         C       D    
            15         C       D    
            17         C       D    
            20         C       D    
      ------------------------------
    
    

---

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            11                      11           
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Failed 
           Class      Failed 
        Attributes    Failed 
          Values      Failed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Mode Mismatches
    
      ================================
       VARIABLE  MODE.BASE  MODE.COMP 
      --------------------------------
       INTEGER   character   numeric  
      --------------------------------
    
    Class Mismatches
    
      ===================================================
       Variable               Base               Compare 
      ---------------------------------------------------
         DATE    c("A_DATE", "b_date", "cDate")   Date   
      ---------------------------------------------------
    
    Attribute Mismatches
    
      =============================================================================
        Variable    Attribute                Base                     Compare      
      -----------------------------------------------------------------------------
         BINARY     something  list(Sepal.Length = c(5.1, 4.9...        NULL       
       CATEGORICAL   levels            c("A", "B", "D")           c("A", "B", "C") 
        DATETIME      label    This is the label for my amazi...        NULL       
      -----------------------------------------------------------------------------
    
    Value Mismatches
    
    Variable: GROUP2
      ==============================
       ..ROWNUMBER..  Base  Compare 
      ------------------------------
             1         47      1    
             2         52      2    
             3         53      3    
             4         38      4    
             5         52      5    
             6         52      6    
             7         54      7    
             8         48      8    
             9         52      9    
            10         46     10    
      ------------------------------
      Showing 10 of 20 observations
    
    Variable: CONTINUOUS
      =======================================
       ..ROWNUMBER..  Base      Compare      
      ---------------------------------------
             1         1    15.6572034240234 
             5         2    26.1530304291068 
             7         3    16.2928917544776 
      ---------------------------------------
    
    Variable: CATEGORICAL
      ==============================
       ..ROWNUMBER..  Base  Compare 
      ------------------------------
             1         D       C    
             2         D       C    
             3         D       C    
             6         D       C    
             7         D       C    
            15         D       C    
            17         D       C    
            20         D       C    
      ------------------------------
    
    

# Ascii - Misc

    Comparison of Base vs Compare
    
    Dataset Summary
      ===================================================================
            Summary                Base                  Compare         
      -------------------------------------------------------------------
        Number of rows              20                      20           
       Number of columns            11                      11           
             Class        data.table, data.frame  data.table, data.frame 
      -------------------------------------------------------------------
    
    Listing of Keys
      ===================
           Variable      
      -------------------
         None Provided   
      -------------------
    
    Check Summary
      =======================
           Name       Result 
      -----------------------
       ExtraRowsBase  Passed 
       ExtraRowsComp  Passed 
       ExtraColsBase  Passed 
       ExtraColsComp  Passed 
           Mode       Passed 
           Class      Passed 
        Attributes    Passed 
          Values      Failed 
      -----------------------
    
    Not all checks have passed, details are provided below
    
    
    Value Mismatches
    
    Variable: CHARACTER
      ==============================
       ..ROWNUMBER..  Base  Compare 
      ------------------------------
             1        <NA>          
      ------------------------------
    
    

