
testthat::test_that("test",{
  testthat::expect_that(C3W4Attempt2::make_filename(2015), testthat::matches("accident_2015.csv.bz2"))
})
