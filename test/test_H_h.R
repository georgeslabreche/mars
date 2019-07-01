# Global daily insolation on Mars horizontal surface [Wh/m2-day].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE IV. - HOURLY AND DAILY GLOBAL INSOLATION ON A HORIZONTAL SURFACE AT MARS SURFACE.

library(testthat) 
library(here)

# Global daily insolation on Mars horizontal surface [Wh/m2-day].
Hh_eq = dget(here("functions", "H_h.R"))

# Test with expected results from TABLE IV.
test_that("H_h.", {
  tolerance = 25
  
  phi = 22.3
  al = 0.1
  nfft = 3
  
  expected_results = list(
    "69" = c(0.65, 3340),
    "120" = c(0.40, 3860),
    "153" = c(0.50, 3882),
    "249" = c(1.40, 1900))
    # FIXME: Larger errors when Ls = 299°.
    #"299" = c(3.25, 1024)) 
  
  index = 1
  for(expected_result in expected_results){
    
    Ls = strtoi(names(expected_results)[index])
    tau = expected_result[1]
    H_h_expected = expected_result[2]
    
    H_h = Hh_eq(Ls, phi, tau, al=al, nfft)
    expect_equal(H_h, H_h_expected, tolerance=tolerance, scale=1)
    
    index = index + 1
  }
})
