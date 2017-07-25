returndata <- function(x,y)
{
DAT <- tibble(testvar.x = x, testvar.y =y , keys=seq(1,length(x)))
DAT
}
x_num <- seq(1,10)
x_num_na <- x_num
x_num_na[10] <- NA
x_floating <-0.58-0.08
x_floating_comp <- 0.5
x_character <- c('antelope', 'bear', 'cake', 'gpro/')
x_character_na <-  c('antelope', 'bear', 'cake', NA)
x_character_one <- 'duck'
x_factor <- factor(c('apple', 'ball','2'))
x_factor_na <- factor(c('apple', 'ball',NA))
x_logical <- c(TRUE,FALSE,TRUE)
x_logical_na <- c(TRUE, FALSE, NA)
x_null <- NULL
x_na <-NA


test_that( "Check comparision of equal objects",{

  expect_true( nrow(identify_variable_diff('testvar', returndata(x_num, x_num), 'keys') ) == 0)
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_num_na, x_num_na), 'keys') ) == 0)
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_floating, x_floating_comp), 'keys') ) == 0)
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_character, x_character), 'keys') ) == 0)
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_character_na, x_character_na), 'keys') ) == 0)
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_character_one, x_character_one), 'keys') ) == 0)
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_factor, x_factor), 'keys') ) == 0)
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_factor_na, x_factor_na), 'keys') ) == 0)
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_logical, x_logical), 'keys') ) == 0)
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_logical_na, x_logical_na), 'keys') ) == 0)
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_na, x_na), 'keys') ) == 0)
})


test_that( "Check comparision of different objects",{
  
  expect_true( nrow(identify_variable_diff('testvar', returndata(x_num, x_num_na), 'keys') ) == 1)

  expect_false( nrow(identify_variable_diff('testvar', returndata(x_floating, x_num), 'keys') ) == 0)
  expect_false( nrow(identify_variable_diff('testvar', returndata(x_character, x_character_na), 'keys') ) == 0)

  expect_false( nrow(identify_variable_diff('testvar', returndata(x_factor, x_factor_na), 'keys') ) == 0)
  
  expect_false( nrow(identify_variable_diff('testvar', returndata(x_logical, x_logical_na), 'keys') ) == 0)

})



