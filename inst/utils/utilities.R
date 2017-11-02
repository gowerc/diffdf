






devtools::document()
devtools::build()
devtools::load_all()

devtools::test()

devtools::check()

# devtools::install()



x <- iris
attr(x$Petal.Length, "rnd") <- "hi"
attr(x$Petal.Length, "Label") <- "BLAH"
x[150,1] <- 200
x <- x[,-5]

rcompare(iris , iris)

COMPARE <- rcompare(iris , x)
print(COMPARE)
print(COMPARE , "Sepal.Length")




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

attr(TDAT2$DATETIME , "label") <- "This is the label for my amazing variable"



rcompare(
    TDAT %>% select(ID , BINARY) ,
    TDAT2 %>% select( ID , BINARY) 
)
)
