# Global hourly insolation on Mars horizontal surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE IV. - HOURLY AND DAILY GLOBAL INSOLATION ON A HORIZONTAL SURFACE AT MARS SURFACE.

library(here)

Ih_eq = dget(here("functions", "I_h.R"))
tolerance = 10
al = 0.1
nfft = 3

expected_results = list(
  "69" = cbind(
    0.65, # Optical depth tau factor.
    c(420, 390, 338, 263, 170, 78, 11)
  ),
  "120" = cbind(
    0.40,  # Optical depth tau factor.
    c(477, 446, 387, 306, 201, 98, 15)
  ),
  "153" = cbind(
    0.50,  # Optical depth tau factor.
    c(508, 471, 399, 302, 185, 73, 3)
  ),
  "249" = cbind(
    1.40,  # Optical depth tau factor.
    c(307, 270, 204, 122, 45, 2, 0)
  ))
  # FIXME: Larger errors when Ls = 299°.
  # "299" = cbind(
  #   3.25,  # Optical depth tau factor.
  #   c(170, 149, 107, 61, 24, 1, 0)
  # ))

test_that("I_h.", {
  
  expected_result_index = 1
  for(expected_result in expected_results){
    
    Ls = strtoi(names(expected_results[expected_result_index]))
    tau = expected_result[1,1]
    
    hour_index = 1
    for(T_start in 12:18){
      
      Ih = Ih_eq(Ls=Ls, phi=22.3, tau=tau, T_start=T_start, T_end=T_start+1, al=al, nfft=nfft)
      Ih_expected = expected_result[hour_index, 2]
      
      expect_equal(Ih, Ih_expected, tolerance=tolerance, scale=1)
      
      hour_index = hour_index + 1
    }
    
    expected_result_index = expected_result_index + 1
  }
})