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
    "tau" = read.csv("data/discretized_tau_at_vl1_fig_3_1991_update.csv"),
    "insolation" =
      list(
        "horizontal" = read.csv("data/daily_insolation_on_horizontal_surface_at_vl1_table_v_update_1990.csv"),
        "beta_equals_phi" = read.csv("data/daily_insolation_on_an_inclined_surface_for_beta_equals_phi_at_vl1_table_ii_1993.csv"),
        "beta_optimal" = read.csv("data/daily_insolation_on_optimal_inclined_angle_beta_at_vl1_table_iii_1993.csv")
      )
  ),
  "VL2" = list(
    "tau" = read.csv("data/discretized_tau_at_vl2_fig_3_1991_update.csv"),
    "insolation" =
      list(
        "horizontal" = read.csv("data/daily_insolation_on_horizontal_surface_at_vl2_table_v_update_1990.csv"),
        "beta_equals_phi" = read.csv("data/daily_insolation_on_an_inclined_surface_for_beta_equals_phi_at_vl2_table_ii_1993.csv"),
        "beta_optimal" = read.csv("data/daily_insolation_on_optimal_inclined_angle_beta_at_vl2_table_iii_1993.csv")
      )
  )
)

#' A convenience function to test daily insolation on horizontal surface.
#' 
#' @param spacecraft The spacecraft data to test against: VL1 or VL2. 
#' @param field The insolation values to test: Hbh, Hdh, or Hh.
#' @param tolerance
#' @param Ls_seq 
#' @param verbose
#'
#' @return
test_daily_insolation_on_horizontal_surface = function(spacecraft, field, tolerance, Ls_seq, verbose=FALSE){
  
  # Spacecraft location.
  phi = spacecrafts[[spacecraft]][["latitude"]]
  longitude = spacecrafts[[spacecraft]][["longitude"]]
  
  # Discretized ground truth optical depth measured by the spacecraft.
  measured_taus = expected_data[[spacecraft]][["tau"]]

  # Expected insolation as per the literature.
  expected_insolations = expected_data[[spacecraft]][["insolation"]][["horizontal"]]
  
  for(Ls in Ls_seq){
    # Measured tau.
    tau_measured = measured_taus[measured_taus$Ls == Ls, "tau"]
    
    # Expected insolation.
    val_expected = expected_insolations[expected_insolations$Ls == Ls, field]
    
    # Calculated insolation.
    val_calculated = ifelse(field=="Hbh", H_bh(Ls=Ls, phi=phi, tau=tau_measured),
                                               ifelse(field=="Hdh", H_dh(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured),
                                                      ifelse(field=="Hh", H_h(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured),
                                                             stop("Invalid field."))))
    
    # Round the calculated value to match floating point numbers in expected value.
    val_calculated = round(val_calculated, 2)
    
    if(isTRUE(verbose)){
      print(paste("Ls:", Ls,
                  " tau:", tau_measured,
                  " ", field, "_calculated:", val_calculated,
                  " ", field, "_expected:", val_expected,
                  " diff:", (val_calculated-val_expected)), sep="")     
    }

    
    # Assert equality.
    expect_equal(val_calculated, val_expected, tolerance=tolerance*val_expected, scale=1)
  }
}


#' Title
#'
#' @param spacecraft The spacecraft data to test against: VL1 or VL2.
#' @param field The insolation values to test: Hbi, Hdi, Hali, or Hi. 
#' @param tolerance 
#' @param Ls_seq 
#' @param beta_equals_phi Test against literature data in which beta = phi. Else use optimal beta angle.
#' @param verbose 
#'
#' @return
test_daily_insolation_on_inclined_surface = function(spacecraft, field, tolerance, Ls_seq, beta_equals_phi=FALSE, verbose=FALSE){
  
  # Spacecraft location.
  phi = spacecrafts[[spacecraft]][["latitude"]]
  longitude = spacecrafts[[spacecraft]][["longitude"]]
  
  # Discretized ground truth optical depth measured by the spacecraft.
  measured_taus = expected_data[[spacecraft]][["tau"]]
  
  expected_insolations = NULL
  if (isTRUE(beta_equals_phi)){
    expected_insolations = expected_data[[spacecraft]][["insolation"]][["beta_equals_phi"]]
  }else{
    expected_insolations = expected_data[[spacecraft]][["insolation"]][["beta_optimal"]]
  }
  
  # Spacecraft orientation.
  gamma_c = 0
  
  # Spacecraft surface inclination angle.
  beta = ifelse(isTRUE(beta_equals_phi),
                spacecrafts[[spacecraft]][["latitude"]],
                spacecrafts[[spacecraft]][["beta_optimal"]])
  
  
  for(Ls in Ls_seq){
    # Measured tau.
    tau_measured = measured_taus[measured_taus$Ls == Ls, "tau"]
    
    # Expected insolation.
    val_expected = expected_insolations[expected_insolations$Ls == Ls, field]
    
    # Calculated insolation.
    val_calculated = ifelse(field=="Hb", H_bi(Ls=Ls, phi=phi, tau=tau_measured, beta=beta, gamma_c=gamma_c),
                            ifelse(field=="Hd", H_di(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, beta=beta, gamma_c=gamma_c),
                                   ifelse(field=="Hal", H_ali(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, beta=beta, gamma_c=gamma_c),
                                          ifelse(field=="H", H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, beta=beta, gamma_c=gamma_c),
                                                 stop("Invalid field.")))))
    
    # Round the calculated valuNULLe to match floating point numbers in expected value.
    val_calculated = round(val_calculated, 1)
    
    if(isTRUE(verbose)){
      print(paste("Ls:", Ls,
                  " tau:", tau_measured,
                  " ", field, "_calculated:", val_calculated,
                  " ", field, "_expected:", val_expected,
                  " diff:", (val_calculated-val_expected)), sep="")     
    }
    
    # Assert equality.
    expect_equal(val_calculated, val_expected, tolerance=tolerance*val_expected, scale=1)
  }
}
