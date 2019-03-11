

context("Testing entire function")


##############################
#
# Set up testing datasets
#
suppressWarnings(RNGversion("3.5.0"))
set.seed(101010223)


#### Change values 
TDAT_INTCHANGE <- TDAT
TDAT_INTCHANGE$INTEGER[[5]] <- 0L

TDAT_CHARCHANGE <- TDAT 
TDAT_CHARCHANGE$CHARACTER[[4]] <- "different"

TDAT_DATECHANGE <- TDAT 
TDAT_DATECHANGE$DATE[[1]] <- as.Date("01/01/1981", format = "%d/%m/%Y")

TDAT_LOGCHANGE <- TDAT 
TDAT_LOGCHANGE$LOGICAL[[1]] <- !TDAT_LOGCHANGE$LOGICAL[[1]] 

TDAT_FACTVALCHANGE <- TDAT  
TDAT_FACTVALCHANGE$CATEGORICAL[TDAT_FACTVALCHANGE$CATEGORICAL == "C"] <- "B"



#### add NAs 
TDAT_CHARCHANGENA <- TDAT 
TDAT_CHARCHANGENA$CHARACTER[[3]] <- NA 

TDAT_DATECHANGENA <- TDAT 
TDAT_DATECHANGENA$DATE[[1]] <- NA

TDAT_LOGCHANGENA <- TDAT 
TDAT_LOGCHANGENA$LOGICAL[[2]] <- NA

TDAT_FACTVALCHANGENA <- TDAT 
TDAT_FACTVALCHANGENA$CATEGORICAL[TDAT_FACTVALCHANGENA$CATEGORICAL == "C"] <- NA



#### add a unsupported column
TDAT_PLUSLIST <- TDAT 
TDAT_PLUSLIST$LIST <- rep(list(TDAT$CATEGORICAL) , nrow(TDAT))

#### add change in mode
TDAT_MODECHANGE <- TDAT 
TDAT_MODECHANGE$INTEGER = as.character(TDAT$INTEGER)

#### Add extra factor levels
TDAT_FACTCHANGE <- TDAT 
TDAT_FACTCHANGE$CATEGORICAL = factor(
    TDAT$CATEGORICAL, 
    levels = c(levels(TDAT$CATEGORICAL), 'New')
)

##change label

TDAT_LABEXT <- TDAT
TDAT_LABEXT2 <- TDAT

attr(TDAT_LABEXT$INTEGER,'label') <- 'Int label'
attr(TDAT_LABEXT$ID,'label') <- 'ID label'

attr(TDAT_LABEXT2$ID,'label') <- 'different label'

### add some extra attributes

TDAT_ATTEXT <- TDAT
TDAT_ATTEXT2 <- TDAT

attr(TDAT_ATTEXT$DATE, 'newatt') <- list(data.frame(x=rnorm(10), y = rnorm(10), a='test'))

attr(TDAT_ATTEXT2$DATE, 'newatt') <- list(B = data.frame(x=rnorm(10), y = rnorm(10), c='test'))

attr(TDAT_ATTEXT2$GROUP2, 'newatt') <- data.frame(x=4, y=5)


#### switch integer to double
TDAT_MODEDBL <- TDAT 
TDAT_MODEDBL$INTEGER <- as.double(TDAT$INTEGER)

#### switch factor to char
TDAT_MODECHR <- TDAT 
TDAT_MODECHR$CATEGORICAL <- as.character(TDAT_MODECHR$CATEGORICAL)

#### add extra columns
TDAT_EXTCOLS <- TDAT
TDAT_EXTCOLS$ext <- TDAT$CATEGORICAL
TDAT_EXTCOLS$ext2 <- TDAT$CATEGORICAL



#### add extra rows
TDAT_EXTROWS <- rbind(TDAT, TDAT)


###################################
#
# Tests
#

expect_length_0 <- function(x){
    expect_length( x , 0 )
}

test_that( "Check comparision of equal objects",{
    expect_length_0( diffdf(iris, iris) )
    expect_length_0( diffdf(TDAT, TDAT))
    expect_length_0( diffdf(TDAT, TDAT, "ID"))
    expect_length_0( diffdf(TDAT, TDAT, c("GROUP1" , "GROUP2")))
    expect_length_0( diffdf(TDAT_CHARCHANGENA , TDAT_CHARCHANGENA ))
    expect_length_0( diffdf(TDAT_DATECHANGENA , TDAT_DATECHANGENA ))
    expect_length_0( diffdf(TDAT_LOGCHANGENA , TDAT_LOGCHANGENA ))
    expect_length_0( diffdf(TDAT_FACTVALCHANGENA, TDAT_FACTVALCHANGENA))
    
    expect_length_0( diffdf(TDAT_LABEXT , TDAT_LABEXT ) )
    expect_length_0( diffdf(TDAT_ATTEXT , TDAT_ATTEXT ) )
    expect_length_0( diffdf(TDAT_FACTCHANGE, TDAT_FACTCHANGE) )
    
    expect_length_0( diffdf(iris, iris, tolerance =0.2, scale=0.1 ))
    expect_length_0( diffdf(TDAT, TDAT, tolerance =0.2, scale=0.1 ))
    expect_length_0( diffdf(TDAT, TDAT, "ID", tolerance =0.2, scale=0.1 ))
    expect_length_0( diffdf(TDAT, TDAT, c("GROUP1" , "GROUP2"), tolerance =0.2, scale=0.1 ) )
    expect_length_0( diffdf(TDAT_CHARCHANGENA , TDAT_CHARCHANGENA , tolerance =0.2, scale=0.1 ) )
    expect_length_0( diffdf(TDAT_DATECHANGENA , TDAT_DATECHANGENA , tolerance =0.2, scale=0.1 )  )
    expect_length_0( diffdf(TDAT_LOGCHANGENA , TDAT_LOGCHANGENA , tolerance =0.2, scale=0.1 ) )
    expect_length_0( diffdf(TDAT_FACTVALCHANGENA, TDAT_FACTVALCHANGENA, tolerance =0.2, scale=0.1 )  )
    
})

test_that( "Unequal objects raise warnings" , {
    
    msg <- "\nNot all Values Compared Equal"
    
    expect_warning( diffdf(TDAT , TDAT_INTCHANGE)       , msg )
    expect_warning( diffdf(TDAT , TDAT_CHARCHANGE )     , msg )
    expect_warning( diffdf(TDAT , TDAT_DATECHANGE )     , msg )
    expect_warning( diffdf(TDAT , TDAT_LOGCHANGE )      , msg )
    expect_warning( diffdf(TDAT , TDAT_FACTVALCHANGE )  , msg )
    expect_warning( diffdf(TDAT , TDAT_CHARCHANGENA )   , msg )
    expect_warning( diffdf(TDAT , TDAT_DATECHANGENA )   , msg )
    expect_warning( diffdf(TDAT , TDAT_LOGCHANGENA )    , msg )
    expect_warning( diffdf(TDAT , TDAT_FACTVALCHANGENA ), msg )
    expect_warning( diffdf(TDAT , TDAT_INTCHANGE, tolerance =0.2, scale=0.1 )       , msg )
    expect_warning( diffdf(TDAT , TDAT_CHARCHANGE, tolerance =0.2, scale=0.1  )     , msg )
    expect_warning( diffdf(TDAT , TDAT_DATECHANGE, tolerance =0.2, scale=0.1  )     , msg )
    expect_warning( diffdf(TDAT , TDAT_LOGCHANGE, tolerance =0.2, scale=0.1  )      , msg )
    expect_warning( diffdf(TDAT , TDAT_FACTVALCHANGE, tolerance =0.2, scale=0.1  )  , msg )
    expect_warning( diffdf(TDAT , TDAT_CHARCHANGENA , tolerance =0.2, scale=0.1 )   , msg )
    expect_warning( diffdf(TDAT , TDAT_DATECHANGENA, tolerance =0.2, scale=0.1  )   , msg )
    expect_warning( diffdf(TDAT , TDAT_LOGCHANGENA , tolerance =0.2, scale=0.1 )    , msg )
    expect_warning( diffdf(TDAT , TDAT_FACTVALCHANGENA, tolerance =0.2, scale=0.1  ), msg )
})


numdiffcheck <-function(compdat, target, value){
    ### Only expected 1 variable to be different thus we expect 
    ### the overall # of differences to equal the # of differences
    ### in the target variable
    diffdf_ob   <- diffdf(TDAT , compdat , suppress_warnings = T )$NumDiff

    expect_true(
        nrow(diffdf_ob) == 1,
        info = 'Too many columns detected as different'
    )
    
    expect_true(
        diffdf_ob$Variable[1] == target,
        info = 'Wrong target!'
    )
    
    expect_equal(
        diffdf_ob$`No of Differences`[1] , value,
        info = 'Number of differences incorrect'
    )
}


test_that( "Unequal object, checking numbers correct" , {
    numdiffcheck( TDAT_CHARCHANGE,      'CHARACTER'  , 1)
    numdiffcheck( TDAT_DATECHANGE,      'DATE'       , 1)
    numdiffcheck( TDAT_LOGCHANGE,       'LOGICAL'    , 1)
    numdiffcheck( TDAT_FACTVALCHANGE,   'CATEGORICAL', 8)
    numdiffcheck( TDAT_CHARCHANGENA,    'CHARACTER'  , sum(is.na(TDAT_CHARCHANGENA$CHARACTER)))
    numdiffcheck( TDAT_DATECHANGENA,    'DATE'       , sum(is.na(TDAT_DATECHANGENA$DATE)))
    numdiffcheck( TDAT_LOGCHANGENA,     'LOGICAL'    , sum(is.na(TDAT_LOGCHANGENA$LOGICAL)))
    numdiffcheck( TDAT_FACTVALCHANGENA, 'CATEGORICAL', sum(is.na(TDAT_FACTVALCHANGENA$CATEGORICAL)))
})


test_that( "Differing modes error" , {
    expect_warning(
        diffdf(TDAT , TDAT_MODECHANGE ),
        'There are columns in BASE and COMPARE with different modes'
    )
    expect_warning(
        diffdf(TDAT , TDAT_MODECHR ),
        'There are columns in BASE and COMPARE with different modes'
    )
    
})

test_that( "Differing classes error" , {
    expect_warning( 
        diffdf(TDAT, TDAT_MODEDBL),
        "There are columns in BASE and COMPARE with different classes"
    )
    
    TEMP <- TDAT
    TEMP$CONTINUOUS <- TEMP$INTEGER
    
    expect_warning(
        diffdf(
            TDAT[,"CONTINUOUS", drop=FALSE], 
            TEMP[,"CONTINUOUS", drop=FALSE] 
        ),
        "There are columns in BASE and COMPARE with different classes"
    )
})






test_that("Non-Unique rows error", {
    expect_error(
        diffdf(TDAT , TDAT , "GROUP1"),
        'BY variables in BASE do not result in unique observations'
    )
})



test_that("Illegal columns error", {
    expect_warning(
        diffdf(TDAT_PLUSLIST, TDAT_PLUSLIST),
        'There are columns in BASE with unsupported modes'
    )
    
    expect_warning(
        diffdf(TDAT_PLUSLIST, TDAT_PLUSLIST),
        'There are columns in COMPARE with unsupported modes'
    )
    
    expect_warning(
        diffdf(TDAT, TDAT_PLUSLIST),
        'There are columns in COMPARE with unsupported modes'
    )
})



test_that("Additional columns error", {
    
    expect_warning(
        diffdf(TDAT, TDAT_EXTCOLS),
        'There are columns in COMPARE that are not in BASE'
    )
    
    expect_warning(
        diffdf(TDAT_EXTCOLS, TDAT),
        'There are columns in BASE that are not in COMPARE'
    )
    
})



test_that("Additional rows error", {
    
    expect_warning(
        diffdf(TDAT, TDAT_EXTROWS),
        'There are rows in COMPARE that are not in BASE'
    )
    
    expect_warning(
        diffdf(TDAT_EXTROWS, TDAT),
        'There are rows in BASE that are not in COMPARE'
    )
    
})

test_that("Bad values for scale or tolerance error", {
    expect_error(
        diffdf(TDAT, TDAT, tolerance = 'bad value'),
        "tolerance' should be numeric"
    )
    expect_error(
        diffdf(TDAT, TDAT, scale = 'bad value'),
        "'scale' should be numeric or NULL"
    )
    expect_error(
        diffdf(TDAT, TDAT_EXTROWS, scale = 'bad value'),
        "'scale' should be numeric or NULL"
    )
    expect_error(
        diffdf(TDAT, TDAT_CHARCHANGE, tolerance = 'bad value'),
        "tolerance' should be numeric"
    )
    
})

test_that('Objets with differing attributes produce the correct warning', {
    warning_msg <- "There are columns in BASE and COMPARE with differing attributes"
    expect_warning(diffdf(TDAT, TDAT_FACTCHANGE), warning_msg)
    expect_warning(diffdf(TDAT, TDAT_ATTEXT), warning_msg)
    expect_warning(diffdf(TDAT, TDAT_ATTEXT2), warning_msg)
    expect_warning(diffdf(TDAT_ATTEXT, TDAT_ATTEXT2), warning_msg)
    expect_warning(diffdf(TDAT, TDAT_LABEXT ), warning_msg)
    expect_warning(diffdf(TDAT, TDAT_LABEXT2 ), warning_msg)
    expect_warning(diffdf(TDAT_LABEXT, TDAT_LABEXT2), warning_msg)
})


test_that('Attribute differnce size is correct!', {
    expect_equal(
        diffdf(TDAT, TDAT_FACTCHANGE , suppress_warnings = T)$AttribDiffs %>% nrow, 
        1
    )
    
    expect_equal(
        diffdf(TDAT, TDAT_ATTEXT , suppress_warnings = T)$AttribDiffs %>% nrow, 
        1
    )
    
    expect_equal(
        diffdf(TDAT, TDAT_ATTEXT2 , suppress_warnings = T)$AttribDiffs %>% nrow, 
        2
    )
    
    expect_equal(
        diffdf(TDAT_ATTEXT, TDAT_ATTEXT2 , suppress_warnings = T)$AttribDiffs %>% nrow, 
        2
    )
    
    expect_equal(
        diffdf(TDAT, TDAT_LABEXT , suppress_warnings = T)$AttribDiffs %>% nrow, 
        2
    )
    
    expect_equal(
        diffdf(TDAT, TDAT_LABEXT2 , suppress_warnings = T)$AttribDiffs %>% nrow, 
        1
    )
    
    expect_equal(
        diffdf(TDAT_LABEXT, TDAT_LABEXT2 , suppress_warnings = T)$AttribDiffs%>% nrow, 
        2
    )
    
})




test_that( "strict_numeric and strict_factor was as intended", {

    ####  Test - Integer and Numeric compare succesfully with strict_numeric = FALSE
    
    expect_length_0(
        suppressMessages(
            diffdf(TDAT_MODEDBL, TDAT, strict_numeric = FALSE)    
        )
    ) 
    
    expect_message(
        diffdf(TDAT_MODEDBL, TDAT, strict_numeric = FALSE),
        "NOTE: Variable INTEGER in compare was casted to numeric", 
        all = TRUE, 
        fixed = TRUE
    )
    
    expect_message(
        diffdf(TDAT, TDAT_MODEDBL, strict_numeric = FALSE),
        "NOTE: Variable INTEGER in base was casted to numeric", 
        all = TRUE, 
        fixed = TRUE
    )
    

    ### Test - Character and Factor compare succesfully with strict_factor = FALSE
    
    expect_length_0(
        suppressMessages(
            diffdf(TDAT_MODECHR, TDAT, strict_factor = FALSE)
        )
    ) 
    
    expect_message(
        diffdf(TDAT_MODECHR, TDAT, strict_factor = FALSE),
        "NOTE: Variable CATEGORICAL in compare was casted to character", 
        all = TRUE, 
        fixed = TRUE
    )
    
    expect_message(
        diffdf(TDAT, TDAT_MODECHR, strict_factor = FALSE),
        "NOTE: Variable CATEGORICAL in base was casted to character", 
        all = TRUE, 
        fixed = TRUE
    )
    
    #### Test - interaction of both options
    
    expect_message(
        diffdf(TDAT, TDAT_MODEDBL, strict_numeric = FALSE, strict_factor = FALSE),
        "NOTE: Variable INTEGER in base was casted to numeric", 
        all = TRUE, 
        fixed = TRUE
    )
    
    expect_message(
        diffdf(TDAT, TDAT_MODECHR, strict_factor = FALSE, strict_numeric = FALSE),
        "NOTE: Variable CATEGORICAL in base was casted to character", 
        all = TRUE, 
        fixed = TRUE
    )
    
    expect_message(
        diffdf(TDAT_MODEDBL, TDAT_MODECHR, strict_factor = FALSE, strict_numeric = FALSE),
        "NOTE: Variable CATEGORICAL in base was casted to character|NOTE: Variable INTEGER in compare was casted to numeric", 
        all = TRUE
    )
    
})












