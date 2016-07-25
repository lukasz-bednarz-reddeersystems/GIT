library(lubridate)
context("DataSet class sanity")

# test vectors
tested.class <- "DataSet"

test_that(paste("Can create object of class", tested.class), {
  expect_is(new(tested.class), tested.class)
})

test_that(paste("Can call getData() on", tested.class), {
  object <- new(tested.class)

  expect_is(object, tested.class)
  expect_is(getData(object), "data.frame")
})

test_that(paste("Can create object of ", tested.class, "using dataset_factory()."), {

  valid.data <- data.frame(Date = today(), Instrument = seq(10))

  object <- dataset_factory(c("Date"), valid.data)

  expect_is(object, tested.class)

  ret.data <- getData(object)

  expect_is(ret.data, "data.frame")
  expect_equal(ret.data, valid.data)

})


test_that(paste("Can call setData() on", tested.class), {
  valid.data <- data.frame(Date = today(), Instrument = seq(10))

  object <- dataset_factory(c("Date"), valid.data)

  expect_is(object, tested.class)

  ret.data <- getData(object)

  expect_is(ret.data, "data.frame")
  expect_equal(ret.data, valid.data)

  valid.data <- data.frame(Date = seq(from = today()-1, to = today(), by = 1),
                           Instrument = sample(seq(10), 10))

  object <- setData(object, valid.data)
  ret.data <- getData(object)

  expect_is(ret.data, "data.frame")
  expect_equal(ret.data, valid.data)

})
