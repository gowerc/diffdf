
library(testthat)
library(diffdf)

suppressWarnings(RNGversion("3.5.0"))
set.seed(20202)

TDAT <- structure(
    list(
        ID = 1:20, 
        
        GROUP1 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
        
        GROUP2 = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L), 
        
        INTEGER = c(38L, 42L, 46L, 42L, 41L, 42L, 42L, 44L, 38L,42L, 36L, 41L, 37L, 40L, 45L, 46L, 39L, 36L, 33L, 41L), 
        
        BINARY = c("M","M", "F", "F", "F", "M", "F", "M", "F", "F", "F", "M", "M", "F", "M", "F", "M", "F", "M", "M"), 
        
        DATE = structure(
            c(
                840.618747138167,  -1717.22089068862, 11365.68388242, 11464.0106819484, 10343.4889736422, 
                23619.0652999056, 28829.9688073897, 13371.8371089244, 15794.7108908474, 
                13412.7986611609, 13926.3764811976, 8120.24457815204, 7357.71990842694, 
                15861.0590113647, 8769.15447226778, 5971.26274086894, 1412.54117098026, 
                15752.669683702, 24639.4106460084, 15266.2501477201
            ), 
            class = "Date"
        ), 
        
        DATETIME = structure(
            c(
                1086469905.35456, 1229329236.42913,1003917417.20277, 1340905821.93883, 1051536747.3315, 
                1181241717.26827, 931557799.971158, 1025872400.31867, 282465156.057939, 707493985.088742, 
                978853846.023329, 1129584278.42386, 879177311.632764, 1002637574.5266, 
                814711241.719801, 957006201.562663, 837301542.755799, 1189309614.12043, 
                1143697218.05151, 918916090.86756
            ), 
            class = c("POSIXct", "POSIXt"), 
            tzone = "UTC"
        ), 
        
        CONTINUOUS = c(
            15.6572034240234, 39.8241832649156, 49.8736351891456, 29.0644612331407, 26.1530304291068, 
            28.8297632350831, 16.2928917544776, 31.0917235563284, 31.7453577608713, 
            34.1449315349695, 40.0351106858045, 17.5857005909688, 32.7320820965685, 
            30.4158010016355, 36.7261722039067, 15.4663356495604, 20.7375168923051, 
            30.0049540254658, 39.5782028659693, 21.2274011103092
        ), 
        
        CATEGORICAL = structure(
            c(3L, 3L, 3L, 1L, 1L, 3L, 3L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 3L, 1L, 3L, 1L, 1L, 3L), 
            .Label = c("A", "B", "C"), 
            class = "factor"
        ), 
        
        LOGICAL = c(
            FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, 
            FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, 
            TRUE, TRUE, FALSE, FALSE
        ), 
        
        CHARACTER = c(
            "knlCkyGngudhQDG7c1S", "WS79tq36Lb39x", "GCrvtl0QiH", "ICSsDEAqiAJq2", "HpCkoxPvlWBxcaN", 
            "bn3I34EvoYbJDScFQ", "6rdaFqOV2mTU", "xPRLB6RO0APlL", "wVl", 
            "DJoblLpQ20S2i", "EAS1pDstlU2", "hjpkwY79ou0n", "lhIxIjBK", 
            "7MmcGgpNQbTCB0", "5kPDzTjL5o", "cyRkPs5IdBO2", "Xo4vqSewCJc", 
            "2LZ4RfA", "RoF1sY15ZSwb9o4U", "ULGFHE1"
        )
    ), 
    row.names = c(NA, -20L), 
    class = c("data.table", "data.frame")
)


VALS <- list(
    int = 1:5, 
    int_na = c(1L, 2L, 3L, 4L, 5L, NA), 
    num = c(1, 2, 3, 4, 5), 
    num_na = c(1, 2, 3, 4, NA), 
    flt = c(0, 0.1, 0.2, 0.3, 0.4), 
    flt2 = c(-1e-12, 0.099999999999, 0.199999999999, 0.299999999999, 0.399999999999), 
    flt3 = c(1e-12, 0.1, 0.2, 0.3, 0.4), 
    flt_calc = c(0, 0.1, 0.2, 0.3, 0.4), 
    chr = c("antelope", "bear", "cake", "gpro/", "^admw"), 
    chr_na = c("antelope", "bear", "cake", NA, "@awd"), 
    chr_one = "duck", 
    fct = structure(
        c(2L, 3L, 1L, 5L, 4L), 
        .Label = c("2", "apple", "ball", "pears", "TRUE"), 
        class = "factor"
    ), 
    fct_na = structure(
        c(1L, 2L, NA, 4L, 3L), 
        .Label = c("apple", "ball", "pears", "TRUE"),
        class = "factor"
    ), 
    lgl = c(TRUE, FALSE, TRUE, TRUE, FALSE), 
    lgl_na = c(TRUE, FALSE, NA, TRUE, FALSE), 
    null = NULL, 
    na = NA, 
    dt = structure(c(15372, -345786, 47243, 16435), class = "Date"), 
    dt_na = structure(c(15372, -345786, NA, 16435), class = "Date"), 
    dtm = structure(c(1325458900, 1362360225, 796874639), class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
    dtm_na = structure(c(1325458900,NA, 796874639), class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
    dtm2 = structure(c(1325458899, 1362358905, 796874639), class = c("POSIXct", "POSIXt"), tzone = "UTC")
)



### Setup test data
TDAT2 <- TDAT

## Unequal values
TDAT2$CONTINUOUS[c(1,5,7)] <- c( 1,2,3)

## Different attributes
attr(TDAT2$BINARY , "something") <- iris

## Different levels
levels(TDAT2$CATEGORICAL) <- c("A", "B" , "D")

## Different class
class( TDAT2$DATE) <-  c("A_DATE" , "b_date" , "cDate")

## Different mode
TDAT2$INTEGER[c(1,5,7)] <- c("1" , "2" , "3")

## Large number of differences
TDAT2$GROUP2 <- rpois(20, 50)

## Different labels
attr(TDAT2$DATETIME , "label") <- "This is the label for my amazing variable"




### Create large list of comparisons
list_of_comparisons <- list(
    "Identical" = list(
        TDAT[ ,"ID"], 
        TDAT2[ , "ID"]
    ), 
    
    "Identical 2" = list(  
        TDAT2[,"ID"], 
        TDAT[, "ID"]
    ),
    
    
    
    "Different Values" = list(
        TDAT[, c("ID","CONTINUOUS")], 
        TDAT2[ ,c("ID","CONTINUOUS")]
    ), 
    
    "Different Values 2" = list(
        TDAT2[,c("ID","CONTINUOUS")], 
        TDAT[,c("ID","CONTINUOUS")]
    ),
    
    
    "Different attributes" = list(
        TDAT[,c("ID" , "BINARY")], 
        TDAT2[,c("ID" , "BINARY")]
    ),
    
    "Different attributes 2" = list(
        TDAT2[, c("ID" , "BINARY")], 
        TDAT[,c("ID" , "BINARY")]
    ),
    
    
    
    "Different Levels" = list(
        TDAT[,c("ID" , "CATEGORICAL")], 
        TDAT2[,c("ID" , "CATEGORICAL")]
    ),
    "Different Levels 2" = list(
        TDAT2[,c("ID" , "CATEGORICAL")], 
        TDAT[,c("ID" , "CATEGORICAL")]
    ),
    
    
    "Different Class" = list(
        TDAT[,c( "ID" , "DATE")], 
        TDAT2[,c( "ID" , "DATE")]
    ),
    "Different Class 2" = list(
        TDAT2[,c("ID" , "DATE")], 
        TDAT[,c("ID" , "DATE")]
    ),
    
    
    
    "Different Modes" = list(
        TDAT[,c( "ID" , "INTEGER")], 
        TDAT2[,c("ID" , "INTEGER")]
    ),
    "Different Modes 2" = list(
        TDAT2[,c( "ID" , "INTEGER")], 
        TDAT[,c("ID" , "INTEGER")]
    ),
    
    
    "Missing Columns" = list(
        TDAT[,c("ID" , "INTEGER", "BINARY")], 
        TDAT2[,c( "ID" , "INTEGER")]
    ),
    
    "Missing Columns 2" = list(
        TDAT2[,c( "ID" , "INTEGER", "BINARY")], 
        TDAT[,c("ID" , "INTEGER")]
    ),
    
    
    "Missing Rows" = list(
        TDAT[,c( "ID" , "GROUP1")]  , 
        TDAT2[ 1:nrow(TDAT2)<=10, c("ID" , "GROUP1")]
    ),
    "Missing Rows 2"  = list( 
        TDAT2[,c( "ID" , "GROUP1")]  , 
        TDAT[1:nrow(TDAT2) <= 10, c("ID" , "GROUP1")]
    ),
    
    
    "everything" = list(
        TDAT,
        TDAT2
    ),
    
    "everything 2"  = list(
        TDAT2,
        TDAT
    ),
    
    "Missing Vs NA"  = list(
        {
            M_TDAT <- TDAT 
            M_TDAT$CHARACTER[1] <- NA
            M_TDAT
        },
        {
            M_TDAT2 <- TDAT 
            M_TDAT2$CHARACTER[1] <- ""
            M_TDAT2
        }
    )
)

