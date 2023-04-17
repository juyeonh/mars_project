library(mars)
dir <- ifelse(.Platform$OS.type=="unix",
              "/Users/santiago/Library/CloudStorage/GoogleDrive-santi9608@gmail.com/My Drive/School/SFU/Upper Division/Stat360Projects/Projects/GH/mars/tests/testthat",
              "C:/Users/guerr/Google Drive/School/SFU/Upper Division/Stat360Projects/Projects/GH/mars/tests/testthat")
load(paste0(dir, "/testmars.RData"))
test_that("mars() returns the correct object", {
  expect_equal(mars(y~.,data=marstestdata,control=testmc), 
               testmars, ignore_attr=TRUE)
})
