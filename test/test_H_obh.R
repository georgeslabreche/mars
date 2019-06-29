library(testthat) 
library(here)

Hobh_eq = dget(here("functions", "H_obh.R"))
tolerance = 10

expected_results = list(
  "69" = 4136,
  "120" = 4442,
  "153" = 4620,
  "249" = 3449,
  "299" = 3350)

test_that("Equation 12 (1990): H_obh.", {
  
  index = 1
  for(Hobh_expected in expected_results){
    Ls = strtoi(names(expected_results)[index])
    Hobh = Hobh_eq(Ls, 22.3)

    expect_equal(Hobh, Hobh_expected, tolerance=tolerance, scale=1)
    
    index = index + 1
  }
})