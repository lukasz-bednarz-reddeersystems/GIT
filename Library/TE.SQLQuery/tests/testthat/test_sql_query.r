context("Test SQLQuery")

########################################
#
# Testing SQLQuery class
#
########################################
tested.class <- "VirtualSQLQuery"
test.class   <- "TestSQLQuery"

test_that(sprintf("Cannot instantiate %s class", tested.class ),{
  expect_error(new(tested.class), regexp = "trying to generate an object from a virtual class")
})


test_that(sprintf("Can inherit from %s class", tested.class),{

  setClass(test.class, contains = tested.class)
  expect_true(isClass(test.class))

})

