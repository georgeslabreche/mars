# Equation 12 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE II. - HOURLY AND DAILY BEAM IN SOLATION ON A HORIZONTAL SURFACE AT TOP OF MARS ATMOSPHERE

library(testthat) 
library(here)

Iobh_eq = dget(here("functions", "I_obh.R"))

phi = 22.3
nfft = 3

tolerance = 9

# Expected hourly I_obh for different areocentric longitudes.
# Test for the 1 hour time ranges at 12-13, 13-14, 14-15, 15-16, 16-17, 17-18, and 18-19.
expected_results = list(
  "69" = c(488, 460, 405, 328, 234, 128, 25),
  "120" = c(528, 497, 437, 353, 249, 134, 23),
  "153" = c(572, 536, 467, 368, 247, 113, 7),
  "249" = c(496, 455, 376, 263, 126, 8, 0),
  "299" = c(478, 439, 364, 257, 127, 19, 0))

test_that("Equation 11 (1990): I_obh.", {
  
  expected_result_index = 1
  for(expected_result in expected_results){
    Ls = strtoi(names(expected_results[expected_result_index]))
    
    hour_index = 1
    for(T_start in 12:18){
      Iobh = Iobh_eq(Ls=Ls, phi=phi, T_start=T_start, T_end=T_start+1, nfft=nfft)
      Iobh_expected = expected_result[hour_index]
      
      expect_equal(Iobh, Iobh_expected, tolerance=tolerance, scale=1)
      
      hour_index = hour_index + 1
    }
    
    expected_result_index = expected_result_index + 1
  }
})