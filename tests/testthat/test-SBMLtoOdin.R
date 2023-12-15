test_that("check presence in reserved library works", {
  expect_equal(in_reserved_lib("a",c("a"="a_1","b"="b_1")), "a_1")
  expect_equal(in_reserved_lib("c",c("a"="a_1","b"="b_1")), "c")
})
test_that("parsing power function works", {
  expect_equal(translate_pow("pow(a,b)"), "(a)^(b)")
  expect_equal(translate_pow("pow(a+3,b)"), "(a+3)^(b)")
  expect_equal(translate_pow("c <- pow(a+3,b)\nd <- pow(10,2+x)"), "c <- (a+3)^(b)\nd <- (10)^(2+x)")
})
#test_that("parsing piecewise function works", {
#  expect_equal(translate_piecewise("pow(a,b)"), "(a)^(b)")
#})
# --> piecewise function is not quite general enough yet
