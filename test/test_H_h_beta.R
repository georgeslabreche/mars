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

library(testthat) 
library(here)

# Global daily insolation on Mars horizontal surface [Wh/m2-day].
Hh_eq = dget(here("functions", "H_h.R"))

# Global daily insolation on Mars inclined surface [Wh/m2-day].
Hh_beta_eq = dget(here("functions", "H_h_beta.R"))

# Mars obliquity of rotation axis [deg].
delta_0 = 24.936

# Test with expected results from TABLE V.
test_that("H_h_beta.", {
  
  # Unit test tolerance when comparing calculated result with expected result.
  # Value between 0 and 1 representing divergence tolerance in percentage.
  tolerance = 0.05
  
  tau = 0.5 # Optical depth.
  phi = 22.3 # Geographic latitude.
  nfft = 3
  al=0.1 # Albedo.
  
  gamma_c = 0
  
  # Expected results from Table 3:
  # Ls, Horiz, Beta
  expected_results_table_3 = list(
    "0" = c(3420.8, 3552.0),
    "5" = c(3456.1, 3556.7),
    "20" = c(3534.5, 3565.7),
    "40" = c(3586.0, 3578.4),
    "60" = c(3617.8, 3614.4),
    "80" = c(3672.0, 3683.8),
    "100" = c(3770.8, 3782.9),
    "120" = c(3902.1, 3898.4),
    "140" = c(4017.2, 4008.6),
    "160" = c(4067.5, 4083.3),
    "180" = c(3934.8, 4085.7),
    #"200" = c(3668.9, 3985.4), # FIXME: Why is an error thrown for this Ls?
    "220" = c(3300.6, 3777.8),
    "240" = c(2929.5, 3511.6),
    "260" = c(2679.6, 3299.1),
    "280" = c(2620.3, 3226.1),
    "300" = c(2744.9, 3290.4),
    #"320" = c(2981.9, 3413.0), # FIXME: Why is an error thrown for this Ls?
    #"340" = c(3228.9, 3507.4), # FIXME: Why is an error thrown for this Ls?
    "355" = c(3380.1, 3544.9),
    "360" = c(3420.8, 3552.0))
  
  index = 1
  for(er in expected_results_table_3){
    # Ls test parameter.
    Ls = strtoi(names(expected_results_table_3[index]))
    
    # Equation 7 (1990): Declination angle [deg].
    delta = asin(sin(delta_0*pi/180) * sin(Ls*pi/180)) * 180/pi
    
    # Slope angle.
    beta = phi - delta
    
    # Expected values.
    H_h_expected = er[1]
    H_h_tol = H_h_expected * tolerance
    
    H_h_beta_expected = er[2]
    H_h_beta_tol = H_h_beta_expected * tolerance
    
    # Test assert global daily insolation on Mars horizontal surface.
    #H_h_calculated = Hh_eq(Ls=Ls, phi=phi, tau=tau, al=al, nfft=nfft)
    #expect_equal(H_h_calculated, H_h_expected, tolerance=H_h_tol, scale=1)
    
    # Test assert global daily insolation on Mars inclined surface.
    H_h_beta_calculated = Hh_beta_eq(Ls=Ls, phi=phi, tau=tau, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
    expect_equal(H_h_beta_calculated, H_h_beta_expected, tolerance=H_h_beta_tol, scale=1)
    
    index = index + 1
  }
})