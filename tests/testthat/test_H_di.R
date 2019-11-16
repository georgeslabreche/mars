context("Diffuse daily insolation on an inclined surface")
source("data.R")

# Constant test parameters.
gamma_c = 0

# Convenience function.
expect_equal_all = function(tol, Ls_seq, phi, longitude, beta, measured_taus, expected_insolations){
  
  for(Ls in Ls_seq){
    # Measured tau.
    tau_measured = measured_taus[measured_taus$Ls == Ls, "tau"]
    
    # Expected Hdh on inclined surface.
    Hdi_expected = expected_insolations[expected_insolations$Ls == Ls, "Hd"]
    
    # Calculated Hdh on inclined surface.
    Hdi_calculated = H_di(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, beta=beta, gamma_c=gamma_c)
    Hdi_calculated = round(Hdi_calculated, 1)
    
    #print(paste("Ls:", Ls, "tau:", tau_measured, "Hdi_c:", Hdi_calculated, "Hdi_e:", Hdi_expected, "diff:", (Hdi_calculated-Hdi_expected)))

    # Assert equality.
    expect_equal(Hdi_calculated, Hdi_expected, tolerance=tol*Hdi_expected, scale=1)
  }  
}

# FIXME: Large tolerance
test_that("H_di: Diffuse daily insolation on an inclined surface for beta = phi at VL1.", {

  # Tolerance in percentage
  tol = 0.29

  Ls_seq = seq(0, 360, 5)

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL1$latitude, longitude=spacecrafts$VL1$longitude, beta=spacecrafts$VL1$latitude,
                   measured_taus=expected_data$VL1$tau, expected_insolations=expected_data$VL1$insolation$beta_equals_phi)
})


test_that("H_di: Diffuse daily insolation on an inclined surface for beta = phi at VL2.", {

  # Tolerance (in percentage).
  tol = 0.12

  # Test input parameters.
  Ls_seq = seq(0, 360, 5)

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL2$latitude, longitude=spacecrafts$VL2$longitude, beta=spacecrafts$VL2$latitude,
                   measured_taus=expected_data$VL2$tau, expected_insolations=expected_data$VL2$insolation$beta_equals_phi)
})

# FIXME: Large tolerance
test_that("H_di: Diffuse daily insolation on optimal inclined surface for beta = 6.5° at VL1.", {

  # Tolerance (in percentage).
  tol = 0.29

  # Test input parameters.
  Ls_seq = seq(0, 355, 5) # Areocentric longitude.

  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL1$latitude, longitude=spacecrafts$VL1$longitude, beta=spacecrafts$VL1$beta_optimal,
                   measured_taus=expected_data$VL1$tau, expected_insolations=expected_data$VL1$insolation$beta_optimal)
})

test_that("H_di: Diffuse daily insolation on optimal inclined surface for beta = 22° at VL2.", {
  
  # Tolerance (in percentage).
  tol = 0.09
  
  # Test input parameters.
  Ls_seq = seq(0, 355, 5) # Areocentric longitude.
  
  # Expect equals all calculations against all expected.
  expect_equal_all(tol=tol, Ls=Ls_seq,
                   phi=spacecrafts$VL2$latitude, longitude=spacecrafts$VL2$longitude, beta=spacecrafts$VL2$beta_optimal,
                   measured_taus=expected_data$VL2$tau, expected_insolations=expected_data$VL2$insolation$beta_optimal)
})
