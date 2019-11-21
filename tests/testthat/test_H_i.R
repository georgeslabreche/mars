# Equation 3 (1994): Global irradiance on Mars inclined surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994). Solar radiation on Mars: Tracking photovoltaic array. Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://ntrs.nasa.gov/?R=19950004977
#
# Expected results taken from:
#   Table 3: Daily insolation in Whr/m2 on a 2 axis tracking surface, horizontal surface (horiz),
#            inclined surface with Beta = phi - delta, and four modes of single axis tracking surfaces
#            at VL1 for clear skies with tau = 0.5.
#
context("Global irradiance on Mars inclined surface")
source("utils.R")

# Disable warnings.
Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

test_that("H_i: Global daily insolation on an inclined surface for beta = phi at VL1.", {

  # Tolerance.
  tolerance = 0.03

  Ls_seq = seq(0, 360, 5)

  # Expect equals all calculations against all expected.
  test_daily_insolation_on_inclined_surface(
    spacecraft = "VL1",
    field = "H",
    tolerance = tolerance,
    Ls_seq = Ls_seq,
    beta_equals_phi = TRUE,
    verbose = FALSE)
})

test_that("H_i: Global daily insolation on an optimal inclined angle beta = 6.5 at VL1", {

  # Tolerance.
  tolerance = 0.03

  # Test input parameters.
  Ls_seq = seq(0, 355, 5) # Areocentric longitude.

  # Expect equals all calculations against all expected.
  test_daily_insolation_on_inclined_surface(
    spacecraft = "VL1",
    field = "H",
    tolerance = tolerance,
    Ls_seq = Ls_seq,
    beta_equals_phi = FALSE,
    verbose = FALSE)

})

test_that("H_i: Global daily insolation on an inclined surface for beta = phi at VL2", {

  # Tolerance.
  tolerance = 0.07

  # Test input parameters.
  Ls_seq = seq(0, 360, 5)

  # Expect equals all calculations against all expected.
  test_daily_insolation_on_inclined_surface(
    spacecraft = "VL2",
    field = "H",
    tolerance = tolerance,
    Ls_seq = Ls_seq,
    beta_equals_phi = TRUE,
    verbose = FALSE)
})

test_that("H_i: Global daily insolation on an optimal inclined angle beta = 22 at VL2", {

  # Tolerance.
  tolerance = 0.04

  # Test input parameters.
  Ls_seq = seq(0, 355, 5) # Areocentric longitude.

  # Expect equals all calculations against all expected.
  test_daily_insolation_on_inclined_surface(
    spacecraft = "VL2",
    field = "H",
    tolerance = tolerance,
    Ls_seq = Ls_seq,
    beta_equals_phi = FALSE,
    verbose = FALSE)
})


