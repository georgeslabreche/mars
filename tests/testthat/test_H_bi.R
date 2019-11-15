context("Beam daily insolation on an inclined surface")
source("data.R")

# Constant test parameters.
gamma_c = 0
nfft = 3

# Convenience function.
expect_equal_all = function(tol, Ls_seq, phi, longitude, beta, measured_taus, expected_insolations){
 
  for(Ls in Ls_seq){
    # Measured tau.
    tau_measured = measured_taus[measured_taus$Ls == Ls, "tau"]
    
    # Expected Hbi on inclined surface.
    Hbi_expected = expected_insolations[expected_insolations$Ls == Ls, "Hb"]

    # Calculated Hbi on inclined surface.
    Hbi_calculated = H_bi(Ls=Ls, phi=phi, tau=tau_measured, beta=beta, gamma_c=gamma_c, nfft=nfft)
    Hbi_calculated = round(Hbi_calculated, 1)
    
    #print(paste("Ls:", Ls, "tau:", tau_measured, "Hbi_c:", Hbi_calculated, "Hbi_e:", Hbi_expected, "diff:", (Hbi_calculated-Hbi_expected)))
    
    # Assert equality.
    expect_equal(Hbi_calculated, Hbi_expected, tolerance=tol*Hbi_expected, scale=1)
  }
}


test_that("H_bi: Beam daily insolation on an inclined surface for beta = phi at VL1.", {

  # Tolerance (in percentage).
  tol = 0.03

  # Test input parameters.
  Ls_seq = seq(0, 0, 5)

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL1$latitude, longitude=spacecrafts$VL1$longitude, beta=spacecrafts$VL1$latitude,
                   measured_taus=expected_data$VL1$tau, expected_insolations=expected_data$VL1$insolation$beta_equals_phi)
})


test_that("H_bi: Beam daily insolation on an inclined surface for beta = phi at VL2.", {

  # Tolerance (in percentage).
  tol = 0.09

  # Test input parameters.
  Ls_seq = seq(0, 360, 5)

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL2$latitude, longitude=spacecrafts$VL2$longitude, beta=spacecrafts$VL2$latitude,
                   measured_taus=expected_data$VL2$tau, expected_insolations=expected_data$VL2$insolation$beta_equals_phi)
})


test_that("H_bi: Beam daily insolation on optimal inclined surface for beta = 6.5° at VL1.", {

  # Tolerance (in percentage).
  tol = 0.03

  # Test input parameters.
  Ls_seq = seq(0, 355, 5) # Areocentric longitude.

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL1$latitude, longitude=spacecrafts$VL1$longitude, beta=spacecrafts$VL1$beta_optimal,
                   measured_taus=expected_data$VL1$tau, expected_insolations=expected_data$VL1$insolation$beta_optimal)
})


test_that("H_bi: Beam daily insolation on optimal inclined surface for beta = 22° at VL2.", {

  # Tolerance (in percentage).
  tol = 0.09

  # Test input parameters.
  Ls_seq = seq(0, 355, 5) # Areocentric longitude.

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL2$latitude, longitude=spacecrafts$VL2$longitude, beta=spacecrafts$VL2$beta_optimal,
                   measured_taus=expected_data$VL2$tau, expected_insolations=expected_data$VL2$insolation$beta_optimal)
})
