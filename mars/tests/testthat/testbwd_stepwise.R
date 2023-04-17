library(mars)
dir <- ifelse(.Platform$OS.type=="unix",
              "/Users/santiago/Library/CloudStorage/GoogleDrive-santi9608@gmail.com/My Drive/School/SFU/Upper Division/Stat360Projects/Projects/GH/mars/tests/testthat",
              "C:/Users/guerr/Google Drive/School/SFU/Upper Division/Stat360Projects/Projects/GH/mars/tests/testthat")
load(paste0(dir, "/testbwd_stepwise.RData"))
test_that("bwd_stepwise() returns the correct object", {
  expect_equal(bwd_stepwise(testfwd,testmc), testbwd)
})

