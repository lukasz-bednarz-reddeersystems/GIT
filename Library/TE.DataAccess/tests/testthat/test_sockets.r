context("ProcessSocket class sanity")

# test vectors
tested.class <- "ProcessSocket"
valid.data   <- "belong to us!"

test_that(paste("Can create object of class", tested.class), {
  expect_is(new(tested.class), tested.class)
})

test_that(paste("Can call getData() on closed", tested.class), {
  object <- new(tested.class)

  expect_is(object, tested.class)
  ret_data <- getDataFromSocket(object)
  expect_is(ret_data, "character")
  expect_equal(ret_data, character())
})

