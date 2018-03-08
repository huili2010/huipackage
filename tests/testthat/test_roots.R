test_that("Distinct roots", {

  roots <- real.roots(1, 7, 12)

  expect_that( roots, is_a("numeric") )
  expect_that( length(roots), equals(2) )
  expect_that( roots[1] < roots[2], is_true() )
})
