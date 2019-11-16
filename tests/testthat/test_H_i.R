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
#   TODO: Test against data from Table 2, with observed VL1 opacities.
context("Global irradiance on Mars inclined surface")
source("data.R")

# Constant test parameters.
gamma_c = 0

# Convenience function.
expect_equal_all = function(tol, Ls_seq, phi, longitude, beta, measured_taus, expected_insolations){
  
  for(Ls in Ls_seq){
    # Measured tau.
    tau_measured = measured_taus[measured_taus$Ls == Ls, "tau"]

    # Expected Hi on inclined surface.
    Hi_expected = expected_insolations[expected_insolations$Ls == Ls, "H"]

    # Calculated Hi on inclined surface.
    Hi_calculated = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, beta=beta, gamma_c=gamma_c)
    Hi_calculated = round(Hi_calculated, 1)

    #print(paste("Ls", Ls, "tau", tau_measured, "H_c:", Hi_calculated, "H_e:", Hi_expected, "diff:", (Hi_calculated-Hi_expected)))

    # Assert equality.
    expect_equal(Hi_calculated, Hi_expected, tolerance=tol*Hi_expected, scale=1)
  }
}


test_that("H_i: Global daily insolation on an inclined surface for beta = phi at VL1.", {

  # Tolerance in percentage
  tol = 0.28

  Ls_seq = seq(0, 360, 5)

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL1$latitude, longitude=spacecrafts$VL1$longitude, beta=spacecrafts$VL1$latitude,
                   measured_taus=expected_data$VL1$tau, expected_insolations=expected_data$VL1$insolation$beta_equals_phi)

})


test_that("H_i: Global daily insolation on an inclined surface for beta = phi at VL2.", {

  # Tolerance (in percentage).
  tol = 0.09

  # Test input parameters.
  Ls_seq = seq(0, 360, 5)

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL2$latitude, longitude=spacecrafts$VL2$longitude, beta=spacecrafts$VL2$latitude,
                   measured_taus=expected_data$VL2$tau, expected_insolations=expected_data$VL2$insolation$beta_equals_phi)
})

test_that("H_i: Global daily insolation on an optimal inclined angle beta = 6.5 at VL1.", {

  # Tolerance (in percentage).
  tol = 0.28

  # Test input parameters.
  Ls_seq = seq(0, 355, 5) # Areocentric longitude.

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL1$latitude, longitude=spacecrafts$VL1$longitude, beta=spacecrafts$VL1$beta_optimal,
                   measured_taus=expected_data$VL1$tau, expected_insolations=expected_data$VL1$insolation$beta_optimal)

})

test_that("H_i: Global daily insolation on an optimal inclined angle beta = 22 at VL2.", {

  # Tolerance (in percentage).
  tol = 0.09

  # Test input parameters.
  Ls_seq = seq(0, 355, 5) # Areocentric longitude.

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL2$latitude, longitude=spacecrafts$VL2$longitude, beta=spacecrafts$VL2$beta_optimal,
                   measured_taus=expected_data$VL2$tau, expected_insolations=expected_data$VL2$insolation$beta_optimal)
})


