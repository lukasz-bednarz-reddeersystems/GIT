context("Testing ReferenceData")

test_that("Cannot Create VirtualReferenceData class", {
          expect_error(new("VirtualReferenceData"))
})
