

context("Testing entire function")


##############################
#
# Set up testing datasets
#

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
TDAT_PLUSLIST <- TDAT %>%
    mutate(LIST = rep(list(CATEGORICAL) , nrow(.)))

#### add change in mode
TDAT_MODECHANGE <- TDAT %>%
    mutate( INTEGER = as.character(INTEGER))

#### Add extra factor levels
TDAT_FACTCHANGE <- TDAT %>%
    mutate( CATEGORICAL = factor(CATEGORICAL, levels = c(levels(CATEGORICAL), 'New')))

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
TDAT_MODEDBL <- TDAT %>%
    mutate( INTEGER = as.double(INTEGER))

#### add extra columns
TDAT_EXTCOLS <- TDAT %>%
    mutate(
        ext = CATEGORICAL,
        ext2 = CATEGORICAL
    )


#### add extra rows
TDAT_EXTROWS <- bind_rows(TDAT, TDAT)


###################################
#
# Tests
#

test_that( "Check comparision of equal objects",{
    expect_false( rcompare(iris, iris)$Issues )
    expect_false( rcompare(TDAT, TDAT)$Issues )
    expect_false( rcompare(TDAT, TDAT, "ID")$Issues )
    expect_false( rcompare(TDAT, TDAT, c("GROUP1" , "GROUP2"))$Issues )
    expect_false( rcompare(TDAT_CHARCHANGENA , TDAT_CHARCHANGENA )$Issues )
    expect_false( rcompare(TDAT_DATECHANGENA , TDAT_DATECHANGENA )$Issues )
    expect_false( rcompare(TDAT_LOGCHANGENA , TDAT_LOGCHANGENA )$Issues )
    expect_false( rcompare(TDAT_FACTVALCHANGENA, TDAT_FACTVALCHANGENA)$Issues )
    
    expect_false( rcompare(TDAT_LABEXT , TDAT_LABEXT )$Issues )
    expect_false( rcompare(TDAT_ATTEXT , TDAT_ATTEXT )$Issues )
    expect_false( rcompare(TDAT_FACTCHANGE, TDAT_FACTCHANGE)$Issues )
    
    expect_false( rcompare(iris, iris, tolerance =0.2, scale=0.1 )$Issues)
    expect_false( rcompare(TDAT, TDAT, tolerance =0.2, scale=0.1 )$Issues)
    expect_false( rcompare(TDAT, TDAT, "ID", tolerance =0.2, scale=0.1 )$Issues)
    expect_false( rcompare(TDAT, TDAT, c("GROUP1" , "GROUP2"), tolerance =0.2, scale=0.1 )$Issues )
    expect_false( rcompare(TDAT_CHARCHANGENA , TDAT_CHARCHANGENA , tolerance =0.2, scale=0.1 )$Issues )
    expect_false( rcompare(TDAT_DATECHANGENA , TDAT_DATECHANGENA , tolerance =0.2, scale=0.1 )$Issues  )
    expect_false( rcompare(TDAT_LOGCHANGENA , TDAT_LOGCHANGENA , tolerance =0.2, scale=0.1 )$Issues )
    expect_false( rcompare(TDAT_FACTVALCHANGENA, TDAT_FACTVALCHANGENA, tolerance =0.2, scale=0.1 )$Issues  )
    
})

test_that( "Unequal objects raise warnings" , {
    
    msg <- "\nNot all Values Compared Equal"
    
    expect_warning( rcompare(TDAT , TDAT_INTCHANGE)       , msg )
    expect_warning( rcompare(TDAT , TDAT_CHARCHANGE )     , msg )
    expect_warning( rcompare(TDAT , TDAT_DATECHANGE )     , msg )
    expect_warning( rcompare(TDAT , TDAT_LOGCHANGE )      , msg )
    expect_warning( rcompare(TDAT , TDAT_FACTVALCHANGE )  , msg )
    expect_warning( rcompare(TDAT , TDAT_CHARCHANGENA )   , msg )
    expect_warning( rcompare(TDAT , TDAT_DATECHANGENA )   , msg )
    expect_warning( rcompare(TDAT , TDAT_LOGCHANGENA )    , msg )
    expect_warning( rcompare(TDAT , TDAT_FACTVALCHANGENA ), msg )
    expect_warning( rcompare(TDAT , TDAT_INTCHANGE, tolerance =0.2, scale=0.1 )       , msg )
    expect_warning( rcompare(TDAT , TDAT_CHARCHANGE, tolerance =0.2, scale=0.1  )     , msg )
    expect_warning( rcompare(TDAT , TDAT_DATECHANGE, tolerance =0.2, scale=0.1  )     , msg )
    expect_warning( rcompare(TDAT , TDAT_LOGCHANGE, tolerance =0.2, scale=0.1  )      , msg )
    expect_warning( rcompare(TDAT , TDAT_FACTVALCHANGE, tolerance =0.2, scale=0.1  )  , msg )
    expect_warning( rcompare(TDAT , TDAT_CHARCHANGENA , tolerance =0.2, scale=0.1 )   , msg )
    expect_warning( rcompare(TDAT , TDAT_DATECHANGENA, tolerance =0.2, scale=0.1  )   , msg )
    expect_warning( rcompare(TDAT , TDAT_LOGCHANGENA , tolerance =0.2, scale=0.1 )    , msg )
    expect_warning( rcompare(TDAT , TDAT_FACTVALCHANGENA, tolerance =0.2, scale=0.1  ), msg )
})


numdiffcheck <-function(compdat, target, value){
    
    ### Only expected 1 variable to be different thus we expect 
    ### the overall # of differences to equal the # of differences
    ### in the target variable
    rcompare_ob   <- rcompare(TDAT , compdat , suppress_warnings = T )$NumDiff$value
    rcompare_targ <- rcompare_ob[target] %>% as.numeric()
    rcompare_all  <- rcompare_ob %>% sum() %>% as.numeric()
    
    expect_false(
        rcompare_targ == 0,
        info = 'targeted value wrong!'
    )
    
    expect_false(
        rcompare_all == 0 ,
        info = 'overall value wrong!'
    )
}

test_that( "Unequal object, checking numbers correct" , {
    numdiffcheck( TDAT_CHARCHANGE,      'CHARACTER')
    numdiffcheck( TDAT_DATECHANGE,      'DATE')
    numdiffcheck( TDAT_LOGCHANGE,       'LOGICAL')
    numdiffcheck( TDAT_FACTVALCHANGE,   'CATEGORICAL')
    numdiffcheck( TDAT_CHARCHANGENA,    'CHARACTER')
    numdiffcheck( TDAT_DATECHANGENA,    'DATE')
    numdiffcheck( TDAT_LOGCHANGENA,     'LOGICAL')
    numdiffcheck( TDAT_FACTVALCHANGENA, 'CATEGORICAL')
})

test_that( "Differing modes error" , {
    expect_warning(
        rcompare(TDAT , TDAT_MODECHANGE ),
        'There are columns in BASE and COMPARE with different modes'
    )
})

test_that( "Differing classes error" , {
    expect_warning( 
        rcompare(TDAT, TDAT_MODEDBL),
        "There are columns in BASE and COMPARE with different classes"
    )
    
    expect_warning( 
        rcompare(TDAT %>% select(CONTINUOUS ), TDAT %>% select(CONTINUOUS = INTEGER)),
        "There are columns in BASE and COMPARE with different classes"
    )
})






test_that("Non-Unique rows error", {
    expect_error(
        rcompare(TDAT , TDAT , "GROUP1"),
        'BY variables in BASE do not result in unique observations'
    )
})



test_that("Illegal columns error", {
    expect_warning(
        rcompare(TDAT_PLUSLIST, TDAT_PLUSLIST),
        'There are columns in BASE with unsupported modes'
    )
    
    expect_warning(
        rcompare(TDAT_PLUSLIST, TDAT_PLUSLIST),
        'There are columns in COMPARE with unsupported modes'
    )
    
    expect_warning(
        rcompare(TDAT, TDAT_PLUSLIST),
        'There are columns in COMPARE with unsupported modes'
    )
})



test_that("Additional columns error", {
    
    expect_warning(
        rcompare(TDAT, TDAT_EXTCOLS),
        'There are columns in COMPARE that are not in BASE'
    )
    
    expect_warning(
        rcompare(TDAT_EXTCOLS, TDAT),
        'There are columns in BASE that are not in COMPARE'
    )
    
})



test_that("Additional rows error", {
    
    expect_warning(
        rcompare(TDAT, TDAT_EXTROWS),
        'There are rows in COMPARE that are not in BASE'
    )
    
    expect_warning(
        rcompare(TDAT_EXTROWS, TDAT),
        'There are rows in BASE that are not in COMPARE'
    )
    
})

test_that("Bad values for scale or tolerance error", {
    expect_error(
        rcompare(TDAT, TDAT, tolerance = 'bad value'),
        "tolerance' should be numeric"
    )
    expect_error(
        rcompare(TDAT, TDAT, scale = 'bad value'),
        "'scale' should be numeric or NULL"
    )
    expect_error(
        rcompare(TDAT, TDAT_EXTROWS, scale = 'bad value'),
        "'scale' should be numeric or NULL"
    )
    expect_error(
        rcompare(TDAT, TDAT_CHARCHANGE, tolerance = 'bad value'),
        "tolerance' should be numeric"
    )
    
})

test_that('Objets with differing attributes produce the correct warning', {
    warning_msg <- "There are columns in BASE and COMPARE with differing attributes"
    expect_warning(rcompare(TDAT, TDAT_FACTCHANGE), warning_msg)
    expect_warning(rcompare(TDAT, TDAT_ATTEXT), warning_msg)
    expect_warning(rcompare(TDAT, TDAT_ATTEXT2), warning_msg)
    expect_warning(rcompare(TDAT_ATTEXT, TDAT_ATTEXT2), warning_msg)
    expect_warning(rcompare(TDAT, TDAT_LABEXT ), warning_msg)
    expect_warning(rcompare(TDAT, TDAT_LABEXT2 ), warning_msg)
    expect_warning(rcompare(TDAT_LABEXT, TDAT_LABEXT2), warning_msg)
    
})

test_that('Attribute differnce size is correct!', {
    expect_equal(nrow(rcompare(TDAT, TDAT_FACTCHANGE)$AttribDiffs$value), 1)
    expect_equal(nrow(rcompare(TDAT, TDAT_ATTEXT)$AttribDiffs$value), 1)
    expect_equal(nrow(rcompare(TDAT, TDAT_ATTEXT2)$AttribDiffs$value), 2)
    expect_equal(nrow(rcompare(TDAT_ATTEXT, TDAT_ATTEXT2)$AttribDiffs$value), 2)
    expect_equal(nrow(rcompare(TDAT, TDAT_LABEXT )$AttribDiffs$value), 2)
    expect_equal(nrow(rcompare(TDAT, TDAT_LABEXT2 )$AttribDiffs$value), 1)
    expect_equal(nrow(rcompare(TDAT_LABEXT, TDAT_LABEXT2)$AttribDiffs$value), 2)
    
})
