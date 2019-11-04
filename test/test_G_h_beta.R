# Equation 3 (1994): Global irradiance on Mars inclined surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994). Solar radiation on Mars: Tracking photovoltaic array. Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://ntrs.nasa.gov/?R=19950004977
#
# Expected results taken from:
#   Table 1: Daily insolation in Whr/m2 (global G, beam B, diffuse D, and albedo A)
#            on a 2 axis tracking surface at VL1 for the observed opacities and for tau = 0.5.  
#
#   Table 2: Daily insolation in Whr/m2 on a 2 axis tracking surface, horizontal surface (horiz),
#            inclined surface with Beta = phi - delta, and four modes of single axis tracking surface
#            at VL1 for the observed opacities.
#
#

library(testthat) 
library(here)

Hbh_eq = dget(here("functions", "H_h_beta.R"))

# Test with expected results from TABLE V.
test_that("H_h_beta.", {
  tolerance = 41
  
  tau = 0.5
  phi = 22.3
  nfft = 3
  
  # Expected results from Table 1:
  # Ls, G_2_axis, B_2_axis, D_2_axis, A_2_axis
  expected_results_table_1 = list(
    "0" = c(4010.8, 2665.7, 1250.0, 95.1),
    "60" = c(4100.9, 2721.5, 1301.5, 77.9),
    "120" = c(4423.2, 2935.3, 1403.8, 84.1),
    "180" = c(4613.4, 3066.3, 1437.8, 109.4),
    "240" = c(3782.1, 2513.5, 1135.8, 132.8),
    "300" = c(3543.8, 2355.1, 1064.2, 124.5),
    "360" = c(4010.8, 2665.7, 1250.0, 95.1))
  
  # Expected results from Table 1:
  # Ls, Horiz, Beta
  expected_results_table_2 = list(
    "0" = c(3262.1, 3355.6),
    "60" = c(3412.3, 3410.0),
    "120" = c(4019.1, 4014.6),
    "180" = c(3637.9, 3721.8),
    "240" = c(1991.0, 1936.2),
    "300" = c(1279.7, 1188.6),
    "360" = c(3262.1, 3355.6))
  
  index = 1
  for(expected_result in expected_results_table_1){
 
    
    index = index + 1
  }
})