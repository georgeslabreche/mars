context("Albedo")

# From Table I in (1991 Update).
test_that("Mars surface albedo.", {
  
  # Test normal inputs.
  expect_equal(albedo(latitude=40, longitude=-70, tau=0.5), 0.265)
  expect_equal(albedo(latitude=-10, longitude=120, tau=0.5), 0.185)
  
  # Test boundaries.
  expect_equal(albedo(latitude=90, longitude=180, tau=0.5), 0.4)
  expect_equal(albedo(latitude=-90, longitude=-180, tau=0.5), 0.375)
  expect_equal(albedo(latitude=0, longitude=0, tau=0.5), 0.2)
  
  # Test rounding to multiple of 10.
  expect_equal(albedo(latitude=22.3, longitude=-49.97, tau=0.5), 0.225) # VL1.
  expect_equal(albedo(latitude=47.7, longitude=134.29, tau=0.5), 0.23) # VL2.
  expect_equal(albedo(latitude=-22, longitude=-166, tau=0.5), 0.225)
  expect_equal(albedo(latitude=85.1, longitude=175.3423, tau=0.5), 0.4)
  
  # Test albedo max out at 0.4 for large tau.
  expect_equal(albedo(latitude=0, longitude=0, tau=5), 0.4)
  
  # Test out of bound errors.
  expect_error(albedo(latitude=91, longitude=-166, tau=0.5))
  expect_error(albedo(latitude=89, longitude=181, tau=0.5))
  expect_error(albedo(latitude=-91, longitude=-166, tau=0.5))
  expect_error(albedo(latitude=89, longitude=-181, tau=0.5))
})