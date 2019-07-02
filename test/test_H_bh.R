# Beam daily insolation on Mars horizontal surface [Wh/m2-day].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE V. - HOURLY AND DAILY BEAM INSOLATION ON A HORIZONTAL SURFACE AT MARS SURFACE.

library(testthat) 
library(here)

Hbh_eq = dget(here("functions", "H_bh.R"))

# Test with expected results from TABLE V.
test_that("H_bh.", {
  tolerance = 41
  
  phi = 22.3
  nfft = 3
  
  expected_results = list(
    "69" = c(0.65, 1768), # FIXME: Larger error (40).
    "120" = c(0.40, 2534),
    "153" = c(0.50, 2308),
    "249" = c(1.40, 314),
    "299" = c(3.25, 12))
  
  index = 1
  for(expected_result in expected_results){
    
    Ls = strtoi(names(expected_results)[index])
    tau = expected_result[1]
    H_bh_expected = expected_result[2]
    
    H_bh = Hbh_eq(Ls=Ls, phi=phi, tau=tau, nfft=nfft)
    expect_equal(H_bh, H_bh_expected, tolerance=tolerance, scale=1)
    
    index = index + 1
  }
})