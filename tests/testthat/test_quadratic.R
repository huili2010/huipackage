test_that("Polynomial must be quadratic", {

  expect_that( real.roots(0, 2, 3), throws_error() )

  expect_that( real.roots(0, 2, 3), throws_error("zero") )

  expect_that( real.roots(0, 2, 3), throws_error("[zZ]ero") )
})
