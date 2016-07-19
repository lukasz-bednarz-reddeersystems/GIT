sourceTo("../lib/mapper.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)


#########################
#
# VirtualMapper Tests
#
#########################

test_that("Cannot create VirtualMapper object", {
  expect_error(new("VirtualMapper"))
})

test_that("Can create TestMapper object", {
  expect_is(new("TestMapper"), "TestMapper")
})


test_that("Cannot setRange() on TestMapper object with invalid input", {
  object <- new("TestMapper")
  expect_is(object, "TestMapper")
  
  invalid.range <- LETTERS[1:3]
  names(invalid.range) <- letters[1:2]
  
  expect_error(setRange(object, invalid.range), regexp = "Attempt to set range to an inappropriate value.")
  
  
  invalid.range <- rep(LETTERS[1:3], 5)
  names(invalid.range) <- letters[1:15]
  
  expect_error(setRange(object, invalid.range), regexp = "Attempt to set range to an inappropriate value.")
  
  
})

test_that("Can setRange() on TestMapper object with valid input", {
  object <- new("TestMapper")
  expect_is(object, "TestMapper")
  
  valid.range <- LETTERS[1:3]
  names(valid.range) <- letters[1:3]
  
  object <- setRange(object, valid.range)
  
  expect_equal(getRange(object), valid.range)
  expect_equal(getDomain(object), names(valid.range))
  
  
  valid.range <- LETTERS[1:3]
  names(valid.range) <- letters[1:3]
  
  object <- setRange(object, valid.range)
  
  expect_equal(getRange(object), valid.range)
  expect_equal(getDomain(object), names(valid.range))
  
  
  valid.range <- LETTERS[4:10]
  names(valid.range) <- letters[4:10]
  
  object <- setRange(object, valid.range)
  
  expect_equal(getRange(object), valid.range)
  expect_equal(getDomain(object), names(valid.range))
  
})


test_that("Can use comparator method", {
  object <- new("TestMapper")
  expect_is(object, "TestMapper")
  
  valid.range <- LETTERS[1:3]
  names(valid.range) <- letters[1:3]
  object <- setRange(object, valid.range)
  
  # compare self equality
  expect_equal(object, object)
  expect_true(object == object)
  
  for (val in valid.range) {
    expect_true(val == object)
    expect_true(object == val)
  }
  
  # compare equality to other object with the same range
  object.2 <- new("TestMapper")
  expect_is(object, "TestMapper")

  object.2 <- setRange(object.2, valid.range)
  
  expect_true(object == object.2)
  expect_true(object.2 == object)
  
  
  
  # compare inequality of overlapping tests
  valid.range <- LETTERS[1:5]
  names(valid.range) <- letters[1:5]
  
  object.2 <- new("TestMapper")
  expect_is(object, "TestMapper")
  
  object.2 <- setRange(object.2, valid.range)
  
  expect_false(object == object.2)
  expect_false(object.2 == object)

  
})


test_that("Can use membership %in% method", {
  object <- new("TestMapper")
  expect_is(object, "TestMapper")
  
  valid.range <- LETTERS[1:3]
  names(valid.range) <- letters[1:3]
  object <- setRange(object, valid.range)
  
  # compare self equality
  expect_true(all(object %in% object))
  
  for (val in valid.range) {
    expect_true(val %in% object)
    expect_equal(object %in% val, getRange(object) %in% val)
  }
  
  
})
