##################################################
## Purpose: test file for this package
## Author:  Helen Zhang
## Date:    2018/10/08
## Version: 0.1.0
## Bugs:    not known yet
##################################################

test_that("getColor return correct color", {
  expect_equal(getColor("A"), "red")
  expect_equal(getColor("G"), "green")
  expect_equal(getColor("C"), "blue")
  expect_equal(getColor("T"), "#608899")
  expect_equal(getColor("U"), "#287c8eff")
  expect_equal(getColor("-"), "black")

})

test_that("draw_compare produce ggplot", {
  expect_is(draw_compare('ACTCGCAATATGVTAGGVVA', "ACTT----TATGCTATGCGC"), "ggplot")
  expect_is(draw_compare("AG-T", "AGCT"), "ggplot")
  expect_is(draw_compare("GAGCGT", "GA-C-T"), "ggplot")
})

test_that("align produce ggplot", {
  expect_is(align('ACTCGCAATATGVTAGGVVA', "ACTTTATGCTATGCGC"), "ggplot")
  expect_is(align("ACT", "AGCT"), "ggplot")
  expect_is(align("GAGCGT", "GACT"), "ggplot")
})
