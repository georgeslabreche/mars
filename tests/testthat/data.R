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
        "beta_equals_phi" = read.csv("data/daily_insolation_on_an_inclined_surface_for_beta_equals_phi_at_vl1_table_ii_1993.csv"),
        "beta_optimal" = read.csv("data/daily_insolation_on_optimal_inclined_angle_beta_at_vl1_table_iii_1993.csv")
      )
  ),
  "VL2" = list(
    "tau" = read.csv("data/discretized_tau_at_vl2_fig_3_1991_update.csv"),
    "insolation" =
      list(
        "beta_equals_phi" = read.csv("data/daily_insolation_on_an_inclined_surface_for_beta_equals_phi_at_vl2_table_ii_1993.csv"),
        "beta_optimal" = read.csv("data/daily_insolation_on_optimal_inclined_angle_beta_at_vl2_table_iii_1993.csv")
      )
  )
)
