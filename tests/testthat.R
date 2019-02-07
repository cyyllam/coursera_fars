library(fars)
testthat::expect_that(make_filename(2013), testthat::is_identical_to("accident_2013.csv.bz2"))
