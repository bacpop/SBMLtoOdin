test_that("check presence in reserved library works", {
  expect_equal(in_reserved_lib("a",c("a"="a_1","b"="b_1")), "a_1")
  expect_equal(in_reserved_lib("c",c("a"="a_1","b"="b_1")), "c")
})
test_that("parsing power function works", {
  expect_equal(translate_pow("pow(a,b)"), "(a)^(b)")
  expect_equal(translate_pow("pow(a+3,b)"), "(a+3)^(b)")
  expect_equal(translate_pow("c <- pow(a+3,b)\nd <- pow(10,2+x)"), "c <- (a+3)^(b)\nd <- (10)^(2+x)")
})
test_that("in_reserved_lib function works", {
  expect_equal(in_reserved_lib("a", c("a"=1, "b" =2)), "1")
  expect_equal(in_reserved_lib("a", c("c"=1, "b" =2)), "a")
})
test_that("importing models from BioModels works", {
  correct_model <- readChar("BIOMD0000000012.R", file.info("BIOMD0000000012.R")$size)
  SBMLtoOdin::importSBMLfromBioModels("BIOMD0000000012","Repressilator.R")
  model_from_function <- readChar("Repressilator.R", file.info("Repressilator.R")$size)
  expect_equal(model_from_function, correct_model)
  unlink("Repressilator.R")
})
test_that("importing models from file works", {
  correct_model <- readChar("BIOMD0000000012.R", file.info("BIOMD0000000012.R")$size)
  SBMLtoOdin::importSBMLfromFile("BIOMD0000000012_url.xml","Repressilator.R")
  model_from_function <- readChar("Repressilator.R", file.info("Repressilator.R")$size)
  expect_equal(model_from_function, correct_model)
  unlink("Repressilator.R")
})
