# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface [W/m2-hr].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE V. - HOURLY AND DAILY BEAM INSOLATION ON A HORIZONTAL SURFACE AT MARS SURFACE.

library(testthat) 
library(here)

Ibh_eq = dget(here("functions", "I_bh.R"))
nfft = 3
tolerance = 10

expected_results = list(
  # Areocentric Longitude.
  "69" = cbind(
    0.65, # Optical depth tau factor.
    c(252, 230, 186, 128, 67, 20, 3) # Expected hourly I_dh from 12:00 to 19:00.
  ),
  "120" = cbind(
    0.40,
    c(352, 322, 265, 190, 103, 33, 2)
  ),
  "153" = cbind(
    0.50,
    c(345, 310, 244, 163, 77, 15, 0)
  ),
  "249" = cbind(
    1.40,
    c(69, 50, 26, 10, 2, 0, 0)
  ),
  "299" = cbind(
    3.25,
    c(3, 2, 1, 0, 0, 0, 0)
  ))

test_that("Equation 19 (1990): I_bh.", {
  
  expected_result_index = 1
  for(expected_result in expected_results){
    Ls = strtoi(names(expected_results[expected_result_index]))
    tau = expected_result[1,1]
    
    hour_index = 1
    for(T_start in 12:18){
    
      Ibh = Ibh_eq(Ls=Ls, phi=22.3, tau=tau, T_start=T_start, T_end=T_start+1, nfft=nfft)
      Ibh_expected = expected_result[hour_index, 2]
      
      expect_equal(Ibh, Ibh_expected, tolerance=tolerance, scale=1)
      
      hour_index = hour_index + 1
    }
    
    expected_result_index = expected_result_index + 1
  }
})


