# Equation 13 (1990): Daily beam insolation on a horizontal surfce at top of Mars atmosphere [Wh/m2-day].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE II. - HOURLY AND DAILY BEAM INSOLATION ON A HORIZONTAL SURFACE AT TOP OF MARS ATMOSPHERE

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

test_that("Equation 13 (1990): H_obh.", {
  
  index = 1
  for(Hobh_expected in expected_results){
    Ls = strtoi(names(expected_results)[index])
    Hobh = Hobh_eq(Ls, 22.3)

    expect_equal(Hobh, Hobh_expected, tolerance=tolerance, scale=1)
    
    index = index + 1
  }
})