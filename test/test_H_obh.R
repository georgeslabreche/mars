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

# Equation 13 (1990): Daily beam insolation on a horizontal surfce at top of Mars atmosphere [Wh/m2-day].
Hobh_eq = dget(here("functions", "H_obh.R"))

# Equation 12 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
Iobh_eq = dget(here("functions", "I_obh.R"))

phi = 22.3
nfft = 3

# Test with expected results from TABLE II.
test_that("Equation 13 (1990): H_obh.", {
  
  tolerance = 10
  
  expected_results = list(
    "69" = 4136,
    "120" = 4442,
    "153" = 4620,
    "249" = 3449,
    "299" = 3350)
  
  index = 1
  for(H_obh_expected in expected_results){
    Ls = strtoi(names(expected_results)[index])
    H_obh = Hobh_eq(Ls, phi)

    expect_equal(H_obh, H_obh_expected, tolerance=tolerance, scale=1)
    
    index = index + 1
  }
})

# H_obh is obtained by integrating I_obh over the period from sunrise to sunset.
# So we can also test if this equality is true.
test_that("Equation 13 (1990): H_obh (compared with I_obh from sunrise to sunset).", {
  tolerance = 1e-10
  
  index = 1
  for(Ls in 0:360){
    H_obh = Hobh_eq(Ls, phi)
    I_obh_day = Iobh_eq(Ls, phi, 0, 24, nfft)
    
    expect_equal(H_obh, I_obh_day, tolerance=tolerance, scale=1)
    
    index = index + 1
  }
})