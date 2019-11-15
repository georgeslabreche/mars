context("Declination")

test_that("Declination (delta).", {
  tolerance = 0
  
  # Ls, Delta
  # TODO: Include more test cases.
  expected_results = list(
    "0" = 0,
    "180" = 0,
    "90" = 24.936,
    "270" = -24.936)
  
  index = 1
  for(delta_expected in expected_results){
    # Ls test parameter.
    Ls = strtoi(names(expected_results[index]))
    
    # Calculate declination [deg].
    delta_calculated = declination(Ls, 2) 
    
    # Test assert equals
    expect_equal(delta_calculated, delta_expected, tolerance=tolerance, scale=1)
    
    index = index + 1
  }
})