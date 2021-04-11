library(dplyr)

rm(list = ls())
devtools::document()
devtools::load_all()


df1 <- iris %>% mutate(KEY1 = row_number(), KEY2 = row_number())
df2 <- iris %>% mutate(KEY1 = row_number(), KEY2 = row_number()) 
df2 <- df2 %>% filter(KEY1 != 125)
class(df2$Sepal.Length) <- c( "test", "test2")
levels(df2$Species) <- c("a", "b", "c")

df2[2,2] <- 999

x <- diffdf(df1, df2, keys = c("KEY1", "KEY2"), onfailure = "warning")


print(x, type = "html")
print(x, type = "ascii")



diffdf( data.frame(), data.frame())

as.character(x, type = "html")

y <- summary(x)
y

y$Class
y$Attributes$VALUES.BASE
y$Attributes$VALUES.COMP
y$Values$Sepal.Width

x$checks$Values$data
    
class(y)

x$checks$Class$data
x$checks$Class$exclude_cols
