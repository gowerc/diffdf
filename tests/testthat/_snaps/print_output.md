# Print output is maintained

    Code
      runme("Identical")
    Output
      No issues were found!

---

    Code
      runme("Identical 2")
    Output
      No issues were found!

---

    Code
      runme("Different Values")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      Not all Values Compared Equal
      All rows are shown in table below
      
        ===============================
          Variable   No of Differences 
        -------------------------------
         CONTINUOUS          3         
        -------------------------------
      
      
      All rows are shown in table below
      
        ==============================================
          VARIABLE   ..ROWNUMBER..    BASE    COMPARE 
        ----------------------------------------------
         CONTINUOUS        1        15.65720     1    
         CONTINUOUS        5        26.15303     2    
         CONTINUOUS        7        16.29289     3    
        ----------------------------------------------
      

---

    Code
      runme("Different Values 2")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      Not all Values Compared Equal
      All rows are shown in table below
      
        ===============================
          Variable   No of Differences 
        -------------------------------
         CONTINUOUS          3         
        -------------------------------
      
      
      All rows are shown in table below
      
        ===========================================
          VARIABLE   ..ROWNUMBER..  BASE  COMPARE  
        -------------------------------------------
         CONTINUOUS        1         1    15.65720 
         CONTINUOUS        5         2    26.15303 
         CONTINUOUS        7         3    16.29289 
        -------------------------------------------
      

---

    Code
      runme("Different attributes")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with differing attributes !!
      All rows are shown in table below
      
        =====================================================================
         VARIABLE  ATTR_NAME  VALUES.BASE             VALUES.COMP            
        ---------------------------------------------------------------------
          BINARY   something     NULL      list(Sepal.Length = c(5.1, 4.9... 
        ---------------------------------------------------------------------
      

---

    Code
      runme("Different attributes 2")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with differing attributes !!
      All rows are shown in table below
      
        =====================================================================
         VARIABLE  ATTR_NAME             VALUES.BASE             VALUES.COMP 
        ---------------------------------------------------------------------
          BINARY   something  list(Sepal.Length = c(5.1, 4.9...     NULL     
        ---------------------------------------------------------------------
      

---

    Code
      runme("Different Levels")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with differing attributes !!
      All rows are shown in table below
      
        ============================================================
          VARIABLE    ATTR_NAME    VALUES.BASE       VALUES.COMP    
        ------------------------------------------------------------
         CATEGORICAL   levels    c("A", "B", "C")  c("A", "B", "D") 
        ------------------------------------------------------------
      
      Not all Values Compared Equal
      All rows are shown in table below
      
        ================================
          Variable    No of Differences 
        --------------------------------
         CATEGORICAL          8         
        --------------------------------
      
      
      All rows are shown in table below
      
        ===========================================
          VARIABLE    ..ROWNUMBER..  BASE  COMPARE 
        -------------------------------------------
         CATEGORICAL        1         C       D    
         CATEGORICAL        2         C       D    
         CATEGORICAL        3         C       D    
         CATEGORICAL        6         C       D    
         CATEGORICAL        7         C       D    
         CATEGORICAL       15         C       D    
         CATEGORICAL       17         C       D    
         CATEGORICAL       20         C       D    
        -------------------------------------------
      

---

    Code
      runme("Different Levels 2")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with differing attributes !!
      All rows are shown in table below
      
        ============================================================
          VARIABLE    ATTR_NAME    VALUES.BASE       VALUES.COMP    
        ------------------------------------------------------------
         CATEGORICAL   levels    c("A", "B", "D")  c("A", "B", "C") 
        ------------------------------------------------------------
      
      Not all Values Compared Equal
      All rows are shown in table below
      
        ================================
          Variable    No of Differences 
        --------------------------------
         CATEGORICAL          8         
        --------------------------------
      
      
      All rows are shown in table below
      
        ===========================================
          VARIABLE    ..ROWNUMBER..  BASE  COMPARE 
        -------------------------------------------
         CATEGORICAL        1         D       C    
         CATEGORICAL        2         D       C    
         CATEGORICAL        3         D       C    
         CATEGORICAL        6         D       C    
         CATEGORICAL        7         D       C    
         CATEGORICAL       15         D       C    
         CATEGORICAL       17         D       C    
         CATEGORICAL       20         D       C    
        -------------------------------------------
      

---

    Code
      runme("Different Class")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with different classes !!
      All rows are shown in table below
      
        ======================================================
         VARIABLE  CLASS.BASE            CLASS.COMP           
        ------------------------------------------------------
           DATE       Date     c("A_DATE", "b_date", "cDate") 
        ------------------------------------------------------
      

---

    Code
      runme("Different Class 2")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with different classes !!
      All rows are shown in table below
      
        ======================================================
         VARIABLE            CLASS.BASE            CLASS.COMP 
        ------------------------------------------------------
           DATE    c("A_DATE", "b_date", "cDate")     Date    
        ------------------------------------------------------
      

---

    Code
      runme("Different Modes")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with different modes !!
      All rows are shown in table below
      
        ================================
         VARIABLE  MODE.BASE  MODE.COMP 
        --------------------------------
         INTEGER    numeric   character 
        --------------------------------
      
      There are columns in BASE and COMPARE with different classes !!
      All rows are shown in table below
      
        ==================================
         VARIABLE  CLASS.BASE  CLASS.COMP 
        ----------------------------------
         INTEGER    integer    character  
        ----------------------------------
      

---

    Code
      runme("Different Modes 2")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with different modes !!
      All rows are shown in table below
      
        ================================
         VARIABLE  MODE.BASE  MODE.COMP 
        --------------------------------
         INTEGER   character   numeric  
        --------------------------------
      
      There are columns in BASE and COMPARE with different classes !!
      All rows are shown in table below
      
        ==================================
         VARIABLE  CLASS.BASE  CLASS.COMP 
        ----------------------------------
         INTEGER   character    integer   
        ----------------------------------
      

---

    Code
      runme("Missing Columns")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with different modes !!
      All rows are shown in table below
      
        ================================
         VARIABLE  MODE.BASE  MODE.COMP 
        --------------------------------
         INTEGER    numeric   character 
        --------------------------------
      
      There are columns in BASE and COMPARE with different classes !!
      All rows are shown in table below
      
        ==================================
         VARIABLE  CLASS.BASE  CLASS.COMP 
        ----------------------------------
         INTEGER    integer    character  
        ----------------------------------
      
      There are columns in BASE that are not in COMPARE !!
      All rows are shown in table below
      
        =========
         COLUMNS 
        ---------
         BINARY  
        ---------
      

---

    Code
      runme("Missing Columns 2")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with different modes !!
      All rows are shown in table below
      
        ================================
         VARIABLE  MODE.BASE  MODE.COMP 
        --------------------------------
         INTEGER   character   numeric  
        --------------------------------
      
      There are columns in BASE and COMPARE with different classes !!
      All rows are shown in table below
      
        ==================================
         VARIABLE  CLASS.BASE  CLASS.COMP 
        ----------------------------------
         INTEGER   character    integer   
        ----------------------------------
      
      There are columns in BASE that are not in COMPARE !!
      All rows are shown in table below
      
        =========
         COLUMNS 
        ---------
         BINARY  
        ---------
      

---

    Code
      runme("Missing Rows")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are rows in BASE that are not in COMPARE !!
      All rows are shown in table below
      
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

    Code
      runme("Missing Rows 2")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are rows in BASE that are not in COMPARE !!
      All rows are shown in table below
      
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

    Code
      runme("everything")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with different modes !!
      All rows are shown in table below
      
        ================================
         VARIABLE  MODE.BASE  MODE.COMP 
        --------------------------------
         INTEGER    numeric   character 
        --------------------------------
      
      There are columns in BASE and COMPARE with different classes !!
      All rows are shown in table below
      
        ======================================================
         VARIABLE  CLASS.BASE            CLASS.COMP           
        ------------------------------------------------------
           DATE       Date     c("A_DATE", "b_date", "cDate") 
         INTEGER    integer              character            
        ------------------------------------------------------
      
      There are columns in BASE and COMPARE with differing attributes !!
      All rows are shown in table below
      
        =============================================================================
          VARIABLE    ATTR_NAME    VALUES.BASE                VALUES.COMP            
        -----------------------------------------------------------------------------
           BINARY     something        NULL        list(Sepal.Length = c(5.1, 4.9... 
         CATEGORICAL   levels    c("A", "B", "C")          c("A", "B", "D")          
          DATETIME      label          NULL        This is the label for my amazi... 
        -----------------------------------------------------------------------------
      
      Not all Values Compared Equal
      All rows are shown in table below
      
        ================================
          Variable    No of Differences 
        --------------------------------
           GROUP2            20         
         CONTINUOUS           3         
         CATEGORICAL          8         
        --------------------------------
      
      
      First 10 of 20 rows are shown in table below
      
        ========================================
         VARIABLE  ..ROWNUMBER..  BASE  COMPARE 
        ----------------------------------------
          GROUP2         1          1     45    
          GROUP2         2          2     46    
          GROUP2         3          3     50    
          GROUP2         4          4     42    
          GROUP2         5          5     43    
          GROUP2         6          6     48    
          GROUP2         7          7     50    
          GROUP2         8          8     62    
          GROUP2         9          9     52    
          GROUP2        10         10     46    
        ----------------------------------------
      
      
      All rows are shown in table below
      
        ==============================================
          VARIABLE   ..ROWNUMBER..    BASE    COMPARE 
        ----------------------------------------------
         CONTINUOUS        1        15.65720     1    
         CONTINUOUS        5        26.15303     2    
         CONTINUOUS        7        16.29289     3    
        ----------------------------------------------
      
      
      All rows are shown in table below
      
        ===========================================
          VARIABLE    ..ROWNUMBER..  BASE  COMPARE 
        -------------------------------------------
         CATEGORICAL        1         C       D    
         CATEGORICAL        2         C       D    
         CATEGORICAL        3         C       D    
         CATEGORICAL        6         C       D    
         CATEGORICAL        7         C       D    
         CATEGORICAL       15         C       D    
         CATEGORICAL       17         C       D    
         CATEGORICAL       20         C       D    
        -------------------------------------------
      

---

    Code
      runme("everything 2")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with different modes !!
      All rows are shown in table below
      
        ================================
         VARIABLE  MODE.BASE  MODE.COMP 
        --------------------------------
         INTEGER   character   numeric  
        --------------------------------
      
      There are columns in BASE and COMPARE with different classes !!
      All rows are shown in table below
      
        ======================================================
         VARIABLE            CLASS.BASE            CLASS.COMP 
        ------------------------------------------------------
           DATE    c("A_DATE", "b_date", "cDate")     Date    
         INTEGER             character              integer   
        ------------------------------------------------------
      
      There are columns in BASE and COMPARE with differing attributes !!
      All rows are shown in table below
      
        =============================================================================
          VARIABLE    ATTR_NAME             VALUES.BASE               VALUES.COMP    
        -----------------------------------------------------------------------------
           BINARY     something  list(Sepal.Length = c(5.1, 4.9...        NULL       
         CATEGORICAL   levels            c("A", "B", "D")           c("A", "B", "C") 
          DATETIME      label    This is the label for my amazi...        NULL       
        -----------------------------------------------------------------------------
      
      Not all Values Compared Equal
      All rows are shown in table below
      
        ================================
          Variable    No of Differences 
        --------------------------------
           GROUP2            20         
         CONTINUOUS           3         
         CATEGORICAL          8         
        --------------------------------
      
      
      First 10 of 20 rows are shown in table below
      
        ========================================
         VARIABLE  ..ROWNUMBER..  BASE  COMPARE 
        ----------------------------------------
          GROUP2         1         45      1    
          GROUP2         2         46      2    
          GROUP2         3         50      3    
          GROUP2         4         42      4    
          GROUP2         5         43      5    
          GROUP2         6         48      6    
          GROUP2         7         50      7    
          GROUP2         8         62      8    
          GROUP2         9         52      9    
          GROUP2        10         46     10    
        ----------------------------------------
      
      
      All rows are shown in table below
      
        ===========================================
          VARIABLE   ..ROWNUMBER..  BASE  COMPARE  
        -------------------------------------------
         CONTINUOUS        1         1    15.65720 
         CONTINUOUS        5         2    26.15303 
         CONTINUOUS        7         3    16.29289 
        -------------------------------------------
      
      
      All rows are shown in table below
      
        ===========================================
          VARIABLE    ..ROWNUMBER..  BASE  COMPARE 
        -------------------------------------------
         CATEGORICAL        1         D       C    
         CATEGORICAL        2         D       C    
         CATEGORICAL        3         D       C    
         CATEGORICAL        6         D       C    
         CATEGORICAL        7         D       C    
         CATEGORICAL       15         D       C    
         CATEGORICAL       17         D       C    
         CATEGORICAL       20         D       C    
        -------------------------------------------
      

---

    Code
      runme("Missing Vs NA")
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      Not all Values Compared Equal
      All rows are shown in table below
      
        ==============================
         Variable   No of Differences 
        ------------------------------
         CHARACTER          1         
        ------------------------------
      
      
      All rows are shown in table below
      
        =========================================
         VARIABLE   ..ROWNUMBER..  BASE  COMPARE 
        -----------------------------------------
         CHARACTER        1        <NA>          
        -----------------------------------------
      

---

    Code
      print(diffdf(list_of_comparisons[["everything"]][[1]], list_of_comparisons[[
        "everything"]][[2]], keys = "ID", suppress_warnings = TRUE))
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with different modes !!
      All rows are shown in table below
      
        ================================
         VARIABLE  MODE.BASE  MODE.COMP 
        --------------------------------
         INTEGER    numeric   character 
        --------------------------------
      
      There are columns in BASE and COMPARE with different classes !!
      All rows are shown in table below
      
        ======================================================
         VARIABLE  CLASS.BASE            CLASS.COMP           
        ------------------------------------------------------
           DATE       Date     c("A_DATE", "b_date", "cDate") 
         INTEGER    integer              character            
        ------------------------------------------------------
      
      There are columns in BASE and COMPARE with differing attributes !!
      All rows are shown in table below
      
        =============================================================================
          VARIABLE    ATTR_NAME    VALUES.BASE                VALUES.COMP            
        -----------------------------------------------------------------------------
           BINARY     something        NULL        list(Sepal.Length = c(5.1, 4.9... 
         CATEGORICAL   levels    c("A", "B", "C")          c("A", "B", "D")          
          DATETIME      label          NULL        This is the label for my amazi... 
        -----------------------------------------------------------------------------
      
      Not all Values Compared Equal
      All rows are shown in table below
      
        ================================
          Variable    No of Differences 
        --------------------------------
           GROUP2            20         
         CONTINUOUS           3         
         CATEGORICAL          8         
        --------------------------------
      
      
      First 10 of 20 rows are shown in table below
      
        =============================
         VARIABLE  ID  BASE  COMPARE 
        -----------------------------
          GROUP2    1    1     45    
          GROUP2    2    2     46    
          GROUP2    3    3     50    
          GROUP2    4    4     42    
          GROUP2    5    5     43    
          GROUP2    6    6     48    
          GROUP2    7    7     50    
          GROUP2    8    8     62    
          GROUP2    9    9     52    
          GROUP2   10   10     46    
        -----------------------------
      
      
      All rows are shown in table below
      
        ===================================
          VARIABLE   ID    BASE    COMPARE 
        -----------------------------------
         CONTINUOUS  1   15.65720     1    
         CONTINUOUS  5   26.15303     2    
         CONTINUOUS  7   16.29289     3    
        -----------------------------------
      
      
      All rows are shown in table below
      
        ================================
          VARIABLE    ID  BASE  COMPARE 
        --------------------------------
         CATEGORICAL   1   C       D    
         CATEGORICAL   2   C       D    
         CATEGORICAL   3   C       D    
         CATEGORICAL   6   C       D    
         CATEGORICAL   7   C       D    
         CATEGORICAL  15   C       D    
         CATEGORICAL  17   C       D    
         CATEGORICAL  20   C       D    
        --------------------------------
      

---

    Code
      print(diffdf(list_of_comparisons[["everything"]][[1]], list_of_comparisons[[
        "everything"]][[2]], keys = c("ID", "GROUP1"), suppress_warnings = TRUE))
    Output
      Differences found between the objects!
      
      A summary is given below.
      
      There are columns in BASE and COMPARE with different modes !!
      All rows are shown in table below
      
        ================================
         VARIABLE  MODE.BASE  MODE.COMP 
        --------------------------------
         INTEGER    numeric   character 
        --------------------------------
      
      There are columns in BASE and COMPARE with different classes !!
      All rows are shown in table below
      
        ======================================================
         VARIABLE  CLASS.BASE            CLASS.COMP           
        ------------------------------------------------------
           DATE       Date     c("A_DATE", "b_date", "cDate") 
         INTEGER    integer              character            
        ------------------------------------------------------
      
      There are columns in BASE and COMPARE with differing attributes !!
      All rows are shown in table below
      
        =============================================================================
          VARIABLE    ATTR_NAME    VALUES.BASE                VALUES.COMP            
        -----------------------------------------------------------------------------
           BINARY     something        NULL        list(Sepal.Length = c(5.1, 4.9... 
         CATEGORICAL   levels    c("A", "B", "C")          c("A", "B", "D")          
          DATETIME      label          NULL        This is the label for my amazi... 
        -----------------------------------------------------------------------------
      
      Not all Values Compared Equal
      All rows are shown in table below
      
        ================================
          Variable    No of Differences 
        --------------------------------
           GROUP2            20         
         CONTINUOUS           3         
         CATEGORICAL          8         
        --------------------------------
      
      
      First 10 of 20 rows are shown in table below
      
        =====================================
         VARIABLE  ID  GROUP1  BASE  COMPARE 
        -------------------------------------
          GROUP2    1    1       1     45    
          GROUP2    2    1       2     46    
          GROUP2    3    1       3     50    
          GROUP2    4    1       4     42    
          GROUP2    5    1       5     43    
          GROUP2    6    1       6     48    
          GROUP2    7    1       7     50    
          GROUP2    8    1       8     62    
          GROUP2    9    1       9     52    
          GROUP2   10    1      10     46    
        -------------------------------------
      
      
      All rows are shown in table below
      
        ===========================================
          VARIABLE   ID  GROUP1    BASE    COMPARE 
        -------------------------------------------
         CONTINUOUS  1     1     15.65720     1    
         CONTINUOUS  5     1     26.15303     2    
         CONTINUOUS  7     1     16.29289     3    
        -------------------------------------------
      
      
      All rows are shown in table below
      
        ========================================
          VARIABLE    ID  GROUP1  BASE  COMPARE 
        ----------------------------------------
         CATEGORICAL   1    1      C       D    
         CATEGORICAL   2    1      C       D    
         CATEGORICAL   3    1      C       D    
         CATEGORICAL   6    1      C       D    
         CATEGORICAL   7    1      C       D    
         CATEGORICAL  15    2      C       D    
         CATEGORICAL  17    2      C       D    
         CATEGORICAL  20    2      C       D    
        ----------------------------------------
      

