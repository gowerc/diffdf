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
      
      
      There are rows in COMPARE that are not in BASE !!
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
      
      
      There are columns in COMPARE that are not in BASE !!
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
      
      
      There are rows in BASE that are not in COMPARE !!
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
      
      
      There are columns in BASE that are not in COMPARE !!
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
      
      
      There are rows in BASE that are not in COMPARE !!
        ===============
         ..ROWNUMBER.. 
        ---------------
               1       
               2       
               3       
        ---------------
      
      
      There are columns in BASE that are not in COMPARE !!
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
      
      
      There are rows in COMPARE that are not in BASE !!
        ===============
         ..ROWNUMBER.. 
        ---------------
               1       
               2       
               3       
        ---------------
      
      
      There are columns in COMPARE that are not in BASE !!
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
      
      
      There are rows in BASE that are not in COMPARE !!
        ====
         ID 
        ----
         A  
        ----
      
      
      There are rows in COMPARE that are not in BASE !!
        ====
         ID 
        ----
         B  
        ----
      
      

# ascii_table can handle all standard datatypes

    Code
      cat(as_ascii_table(TDAT))
    Output
        =================================================================================================================================
         ID  GROUP1  GROUP2  INTEGER  BINARY     DATE            DATETIME          CONTINUOUS  CATEGORICAL  LOGICAL       CHARACTER      
        ---------------------------------------------------------------------------------------------------------------------------------
          1    1        1      38       M     1972-04-20  2004-06-05 21:11:45 UTC   15.65720        C        FALSE   knlCkyGngudhQDG7c1S 
          2    1        2      42       M     1965-04-19  2008-12-15 08:20:36 UTC   39.82418        C        FALSE      WS79tq36Lb39x    
          3    1        3      46       F     2001-02-12  2001-10-24 09:56:57 UTC   49.87364        C        TRUE        GCrvtl0QiH      
          4    1        4      42       F     2001-05-22  2012-06-28 17:50:21 UTC   29.06446        A        TRUE       ICSsDEAqiAJq2    
          5    1        5      41       F     1998-04-27  2003-04-28 13:32:27 UTC   26.15303        A        FALSE     HpCkoxPvlWBxcaN   
          6    1        6      42       M     2034-09-01  2007-06-07 18:41:57 UTC   28.82976        C        FALSE    bn3I34EvoYbJDScFQ  
          7    1        7      42       F     2048-12-06  1999-07-09 22:03:19 UTC   16.29289        C        TRUE       6rdaFqOV2mTU     
          8    1        8      44       M     2006-08-11  2002-07-05 12:33:20 UTC   31.09172        A        FALSE      xPRLB6RO0APlL    
          9    1        9      38       F     2013-03-30  1978-12-14 06:32:36 UTC   31.74536        B        TRUE            wVl         
         10    1       10      42       F     2006-09-21  1992-06-02 14:06:25 UTC   34.14493        A        FALSE      DJoblLpQ20S2i    
         11    2        1      36       F     2008-02-17  2001-01-07 07:50:46 UTC   40.03511        B        TRUE        EAS1pDstlU2     
         12    2        2      41       M     1992-03-26  2005-10-17 21:24:38 UTC   17.58570        B        TRUE       hjpkwY79ou0n     
         13    2        3      37       M     1990-02-22  1997-11-10 15:55:11 UTC   32.73208        A        FALSE        lhIxIjBK       
         14    2        4      40       F     2013-06-05  2001-10-09 14:26:14 UTC   30.41580        B        FALSE     7MmcGgpNQbTCB0    
         15    2        5      45       M     1994-01-04  1995-10-26 12:40:41 UTC   36.72617        C        FALSE       5kPDzTjL5o      
         16    2        6      46       F     1986-05-08  2000-04-29 11:03:21 UTC   15.46634        A        TRUE       cyRkPs5IdBO2     
         17    2        7      39       M     1973-11-13  1996-07-13 23:45:42 UTC   20.73752        C        TRUE        Xo4vqSewCJc     
         18    2        8      36       F     2013-02-16  2007-09-09 03:46:54 UTC   30.00495        A        TRUE          2LZ4RfA       
         19    2        9      33       M     2037-06-17  2006-03-30 05:40:18 UTC   39.57820        A        FALSE    RoF1sY15ZSwb9o4U   
         20    2       10      41       M     2011-10-19  1999-02-13 14:28:10 UTC   21.22740        C        FALSE         ULGFHE1       
        ---------------------------------------------------------------------------------------------------------------------------------

# datetimes compare as expected

    Code
      print(res)
    Output
      Differences found between the objects!
      
      Summary of BASE and COMPARE
        ==================================================================
          PROPERTY             BASE                       COMP            
        ------------------------------------------------------------------
            Name                d1                         d2             
           Class     "tbl_df, tbl, data.frame"  "tbl_df, tbl, data.frame" 
          Rows(#)                2                          2             
         Columns(#)              2                          2             
        ------------------------------------------------------------------
      
      
      There are columns in BASE and COMPARE with differing attributes !!
        ===============================================
         VARIABLE  ATTR_NAME  VALUES.BASE  VALUES.COMP 
        -----------------------------------------------
           dt1       tzone        EST          CET     
        -----------------------------------------------
      
      
      Not all Values Compared Equal
        =============================
         Variable  No of Differences 
        -----------------------------
           dt1             2         
        -----------------------------
      
      
        ================================================================
         VARIABLE  id           BASE                    COMPARE         
        ----------------------------------------------------------------
           dt1     1   2024-01-10 01:02:03 EST  2024-01-10 01:02:03 CET 
           dt1     2   2024-01-24 14:12:49 EST  2024-01-24 14:12:49 CET 
        ----------------------------------------------------------------
      
      

---

    Code
      print(res)
    Output
      Differences found between the objects!
      
      Summary of BASE and COMPARE
        ==================================================================
          PROPERTY             BASE                       COMP            
        ------------------------------------------------------------------
            Name                d1                         d2             
           Class     "tbl_df, tbl, data.frame"  "tbl_df, tbl, data.frame" 
          Rows(#)                2                          2             
         Columns(#)              2                          2             
        ------------------------------------------------------------------
      
      
      There are columns in BASE and COMPARE with differing attributes !!
        ===============================================
         VARIABLE  ATTR_NAME  VALUES.BASE  VALUES.COMP 
        -----------------------------------------------
           dt1       tzone        EST          CET     
        -----------------------------------------------
      
      

