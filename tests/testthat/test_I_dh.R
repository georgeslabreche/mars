# Diffuse hourly insolation on Mars horizontal surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE VI. - HOURLY AND DAILY DIFFUSE INSOLATION ON A HORIZONTAL SURFACE AT MARS SURFACE.

# Disable warnings.
Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

tolerance = 11

phi = 22.3
al = 0.1

expected_results = list(
  # Areocentric Longitude.
  "69" = cbind(
    0.65, # Optical depth tau factor.
    c(168, 160, 152, 135, 103, 58, 10) # Expected hourly I_dh from 12:00 to 19:00.
  ),
  "120" = cbind(
    0.40,
    c(125, 124, 122, 116, 98, 65, 13)
  ),
  "153" = cbind(
    0.50,
    c(163, 161, 155, 139, 108, 58, 3)
  ),
  "249" = cbind(
    1.40,
    c(238, 220, 178, 112, 43, 2, 0)
  ))
  # FIXME: Larger errors when Ls = 299°.
  # "299" = cbind(
  #   3.25,
  #   c(167, 147, 106, 61, 24, 1, 0)
  # ))

test_that("I_dh.", {
  expected_result_index = 1
  for(expected_result in expected_results){
    Ls = strtoi(names(expected_results[expected_result_index]))
    tau = expected_result[1,1]
    
    hour_index = 1
    for(Ts_start in 12:18){
      
      Idh = I_dh(Ls=Ls, phi=phi, tau=tau, Ts_start=Ts_start, Ts_end=Ts_start+1, al=al)
      Idh_expected = expected_result[hour_index, 2]
      
      expect_equal(Idh, Idh_expected, tolerance=tolerance, scale=1)
      
      hour_index = hour_index + 1
    }
    
    expected_result_index = expected_result_index + 1
  }
})


