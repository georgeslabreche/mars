context("Beta optimal")

test_that("Beta optimal: clear day.", {
  beta = optimal_angle(Ls=180, phi=22, unit=2)
  print(beta)
  expect_equal(45, 45, tolerance=1, scale=1)
})