
# Spacecraft properties.
spacecrafts = list(
  "VL1" = list(
    "latitude" = 22.3,
    "longitude" = -49.97,
    "beta_optimal" = 6.5
  ),
  "VL2" = list(
    "latitude" = 47.7,
    "longitude" = 134.29,
    "beta_optimal" = 22
  )
)

# Expected data
expected_data = list(
  "VL1" = list(
    "tau" = read.csv("tests/testthat/data/discretized_tau_at_vl1_fig_3_1991_update.csv"),
    "insolation" =
      list(
        "beta_equals_phi" = read.csv("tests/testthat/data/daily_insolation_on_an_inclined_surface_for_beta_equals_phi_at_vl1_table_ii_1993.csv"),
        "beta_optimal" = read.csv("tests/testthat/data/daily_insolation_on_optimal_inclined_angle_beta_at_vl1_table_iii_1993.csv")
      )
  ),
  "VL2" = list(
    "tau" = read.csv("tests/testthat/data/discretized_tau_at_vl2_fig_3_1991_update.csv"),
    "insolation" =
      list(
        "beta_equals_phi" = read.csv("tests/testthat/data/daily_insolation_on_an_inclined_surface_for_beta_equals_phi_at_vl2_table_ii_1993.csv"),
        "beta_optimal" = read.csv("tests/testthat/data/daily_insolation_on_optimal_inclined_angle_beta_at_vl2_table_iii_1993.csv")
      )
  )
)



play = function(title=title, Ls_seq, phi, longitude, beta, measured_taus, expected_insolations){
  Hbi_e = c()
  Hbi_c = c()
  taus = c()
  diffs = c()
  diffs_pc = c()
  
  for(Ls in Ls_seq){
    # Measured tau.
    tau_measured = measured_taus[measured_taus$Ls == Ls, "tau"]
    
    # Expected Hbi on inclined surface.
    Hbi_expected = expected_insolations[expected_insolations$Ls == Ls, "Hb"]
    
    # Calculated Hbi on inclined surface.
    Hbi_calculated = H_bi(Ls=Ls, phi=phi, tau=tau_measured, beta=beta, gamma_c=gamma_c)
    Hbi_calculated = round(Hbi_calculated, 1)
    
    Hbi_e = c(Hbi_e, Hbi_expected)
    Hbi_c = c(Hbi_c, Hbi_calculated)
    taus = c(taus, tau_measured)
    
    diff = Hbi_expected-Hbi_calculated
    diffs = c(diffs, diff)
    
    diff_pc = (Hbi_expected-Hbi_calculated) / Hbi_calculated
    diffs_pc = c(diffs_pc, diff_pc)
    
    # if(diff_pc > 0.04){
    #   print(paste("Ls", Ls, "tau", tau_measured, "Hbi_e", Hbi_expected, "Hbi_c", Hbi_calculated, "diff_pc", diff_pc))
    # }
  }
  
  dev.new()
  par(mfrow=c(2,2))
  
  plot(Ls_seq, Hbi_e, col="red", type="l", main=title)
  lines(Ls_seq, Hbi_c, col="blue")
  
  plot(Ls_seq, taus, col="orange", type="l", main=title)
  
  plot(taus, diffs, col="red", ylab="UNDERESTIMATED ---->", main=title)
  
  plot(taus, diffs_pc, col="red", ylab="UNDERESTIMATED ---->", main=title)
}

gamma_c = 0


Ls_seq = seq(0, 360, 5)
play(title="VL1, beta = phi", Ls_seq=Ls_seq,
     phi=spacecrafts$VL1$latitude, longitude=spacecrafts$VL1$longitude, beta=spacecrafts$VL1$latitude,
     measured_taus=expected_data$VL1$tau, expected_insolations=expected_data$VL1$insolation$beta_equals_phi)

# play(title="VL2, beta = phi", Ls_seq=Ls_seq,
#      phi=spacecrafts$VL2$latitude, longitude=spacecrafts$VL2$longitude, beta=spacecrafts$VL2$latitude,
#      measured_taus=expected_data$VL2$tau, expected_insolations=expected_data$VL2$insolation$beta_equals_phi)


Ls_seq = seq(0, 355, 5)
play(title="VL1, beta optimal", Ls_seq=Ls_seq,
     phi=spacecrafts$VL1$latitude, longitude=spacecrafts$VL1$longitude, beta=spacecrafts$VL1$beta_optimal,
     measured_taus=expected_data$VL1$tau, expected_insolations=expected_data$VL1$insolation$beta_optimal)
# 
# play(title="VL2, beta optimal", Ls_seq=Ls_seq,
#      phi=spacecrafts$VL2$latitude, longitude=spacecrafts$VL2$longitude, beta=spacecrafts$VL2$beta_optimal,
#      measured_taus=expected_data$VL2$tau, expected_insolations=expected_data$VL2$insolation$beta_optimal)







