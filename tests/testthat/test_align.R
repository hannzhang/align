
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
})

test_that("align produce ggplot", {
  expect_is(align('ACTCGCAATATGVTAGGVVA', "ACTTTATGCTATGCGC"), "ggplot")
})
