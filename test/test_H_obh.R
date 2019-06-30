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
Iobh_eq = dget(here("functions", "I_obh.R"))

# Test with expected results from TABLE II.
tolerance = 10
test_that("Equation 13 (1990): H_obh.", {
  
  expected_results = list(
    "69" = 4136,
    "120" = 4442,
    "153" = 4620,
    "249" = 3449,
    "299" = 3350)
  
  index = 1
  for(Hobh_expected in expected_results){
    Ls = strtoi(names(expected_results)[index])
    Hobh = Hobh_eq(Ls, 22.3)

    expect_equal(Hobh, Hobh_expected, tolerance=tolerance, scale=1)
    
    index = index + 1
  }
})

# Hobh is obtained by integrating Iobh over the period from sunrise to sunset.
# So we can also test if this equality is true.
tolerance = 1e-10
test_that("Equation 13 (1990): H_obh (compared with I_obj from sunrise to sunset).", {
  
  index = 1
  for(Ls in 0:360){
    Hobh = Hobh_eq(Ls, 22.3)
    Iobh_day = Iobh_eq(Ls, 22.3, 0, 24)
    
    expect_equal(Hobh, Iobh_day, tolerance=tolerance, scale=1)
    
    index = index + 1
  }
})