






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
