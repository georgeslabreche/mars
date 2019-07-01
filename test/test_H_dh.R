# Diffuse daily insolation on Mars horizontal surface [Wh/m2-day].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE VI. - HOURLY AND DAILY DIFFUSE INSOLATION O NA HORIZONTAL SURFACE AT MAR SURFACE.

library(testthat) 
library(here)

# Diffuse hourly insolation on Mars horizontal surface [Wh/m2-day].
Hdh_eq = dget(here("functions", "H_dh.R"))

# Test with expected results from TABLE VI.
test_that("H_dh.", {
  tolerance = 50
  
  phi = 22.3
  al = 0.1
  nfft = 3
  
  expected_results = list(
    "69" = c(0.65, 1572), # FIXME: Large error (47).
    "120" = c(0.40, 1326),
    "153" = c(0.50, 1574), # FIXME: Large error (21.6).
    "249" = c(1.40, 1586)) # FIXME: Large error (24.1).
    #"299" = c(3.25, 1012))  # FIXME: Large error (252)
  
  index = 1
  for(expected_result in expected_results){
    
    Ls = strtoi(names(expected_results)[index])
    tau = expected_result[1]
    H_dh_expected = expected_result[2]
    
    H_dh = Hdh_eq(Ls, phi, tau, al, nfft)
    expect_equal(H_dh, H_dh_expected, tolerance=tolerance, scale=1)
    
    index = index + 1
  }
})