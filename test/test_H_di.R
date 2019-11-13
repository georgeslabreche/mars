library(testthat) 
library(here)

Hdi_eq = dget(here("functions", "H_di.R"))

# Data filenames.
filename_tau_vl1 = "discretized_tau_at_vl1_fig_3_1991_update.csv"
filename_tau_vl2 = "discretized_tau_at_vl2_fig_3_1991_update.csv"

filename_insolation_beta_equals_phi_vl1 = "daily_insolation_on_an_inclined_surface_for_beta_equals_phi_at_vl1_table_ii_1993.csv"
filename_insolation_beta_equals_phi_vl2 = "daily_insolation_on_an_inclined_surface_for_beta_equals_phi_at_vl2_table_ii_1993.csv"

filename_insolation_optimal_bela_vl1 = "daily_insolation_on_optimal_inclined_angle_beta_at_vl1_table_iii_1993.csv"
filename_insolation_optimal_bela_vl2 = "daily_insolation_on_optimal_inclined_angle_beta_at_vl2_table_iii_1993.csv"

# Constant test parameters.
gamma_c = 0
nfft = 3

# FIXME: is_irradiated is not taking into account beta and gamma_c values...
expect_equal_all = function(tolerance, Ls_seq, phi, longitude, beta, measured_taus, expected_insolations){
  for(Ls in Ls_seq){
    # Measured tau.
    tau_measured = measured_taus[measured_taus$Ls == Ls, ]$tau
    
    # Expected Hdh on inclined surface.
    Hdi_expected = expected_insolations[expected_insolations$Ls == Ls, ]$Hd
    
    # Calculated Hdh on inclined surface.
    Hdi_calculated = Hdi_eq(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, beta=beta, gamma_c=gamma_c, nfft=nfft)
    Hdi_calculated = round(Hdi_calculated, 1)
    
    print(paste("Ls:", Ls, "tau:", tau_measured, "Hdi_c:", Hdi_calculated, "Hdi_e:", Hdi_expected, "diff:", (Hdi_calculated-Hdi_expected)))

    # Assert equality.
    expect_equal(Hdi_calculated, Hdi_expected, tolerance=tolerance, scale=1)
  }  
}


test_that("H_di: Diffuse daily insolation on an inclined surface for beta = phi at VL1.", {

  tolerance = 0.5

  Ls_seq = seq(0, 360, 5)
  phi_VL1 = 22.3
  longitude_VL1 = -49.97

  # Load VL1 expected data.
  measured_taus = read.csv(here("test/data/", filename_tau_vl1))
  expected_insolations = read.csv(here("test/data/", filename_insolation_beta_equals_phi_vl1))

  # Expect equals all calculations against all expected.
  expect_equal_all(tolerance=tolerance, Ls=Ls_seq, phi=phi_VL1, longitude=longitude_VL1, beta=phi_VL1, measured_taus=measured_taus, expected_insolations=expected_insolations)

})


# test_that("H_di: Diffuse daily insolation on an inclined surface for beta = phi at VL2.", {
# 
#   tolerance = 0.5
# 
#   Ls_seq = seq(0, 360, 5)
#   phi_VL2 = 47.7
#   longitude_VL2 = 134.29
# 
#   # Load VL1 expected data.
#   measured_taus = read.csv(here("test/data/", filename_tau_vl2))
#   expected_insolations = read.csv(here("test/data/", filename_insolation_beta_equals_phi_vl2))
# 
#   # Expect equals all calculations against all expected.
#   expect_equal_all(tolerance=tolerance, Ls=Ls_seq, phi=phi_VL2, longitude=longitude_VL2, beta=phi_VL2, measured_taus=measured_taus, expected_insolations=expected_insolations)
# 
# })