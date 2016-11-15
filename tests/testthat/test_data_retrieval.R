library(acs)
context("Integration test for downloading tiny datasets.")

# If empty keys ever stop working we could add a snippet to skip the tests if an api key hasn't been installed.
# Could also skip the tests if no network is available.

test_that("acs_dp download works", {
     dat = acs.fetch(
          endyear=2013, span = 5, key='', 
          geography=geo.make(zip.code='94709'),
          variable=c("DP02_0069"),
          dataset="acs_dp")
     expect_equal(estimate(dat)[1,1], 249)
     expect_equal(standard.error(dat)[1,1], 52.27964, tolerance=1e-4)
})

test_that("acs download works", {
     dat = acs.fetch(
          endyear=2013, span = 5, key='', 
          geography=geo.make(zip.code='94709'),
          variable=c("B01003_001"),
          dataset="acs")
     expect_equal(estimate(dat)[1,1], 12360)
     expect_equal(standard.error(dat)[1,1], 397.5684, tolerance=1e-4)
})

test_that("sf1 download works", {
     dat = acs.fetch(
          endyear=2000, key='', 
          geography=geo.make(state='CA'),
          variable=c("P028E002"),
          dataset="sf1")
     expect_equal(estimate(dat)[1,1], 37339)
     expect_equal(standard.error(dat)[1,1], 0, tolerance=1e-4)
})

test_that("sf3 download works", {
     dat = acs.fetch(
          endyear=2000, key='', 
          geography=geo.make(state='CA'),
          variable=c("PCT022035"),
          dataset="sf3")
     expect_equal(estimate(dat)[1,1], 88723)
     expect_equal(standard.error(dat)[1,1], 0, tolerance=1e-4)
})