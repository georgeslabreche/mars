context("Diffuse daily insolation on an inclined surface")
source("utils.R")

# Disable warnings.
Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

test_that("H_di: Diffuse daily insolation on an inclined surface for beta = phi at VL1.", {

  # Tolerance.
  tolerance = 0.035

  Ls_seq = seq(0, 360, 5)

  # Expect equals all calculations against all expected.
  test_daily_insolation_on_inclined_surface(
    spacecraft = "VL1",
    field = "Hd",
    tolerance = tolerance,
    Ls_seq = Ls_seq,
    beta_equals_phi = TRUE,
    verbose = FALSE)
})

# test_that("H_di: Diffuse daily insolation on optimal inclined surface for beta = 6.5° at VL1.", {
#   
#   # Tolerance.
#   tolerance = 0.03
#   
#   # Test input parameters.
#   Ls_seq = seq(0, 355, 5) # Areocentric longitude.
#   
#   # Expect equals all calculations against all expected.
#   test_daily_insolation_on_inclined_surface(
#     spacecraft = "VL1",
#     field = "Hd",
#     tolerance = tolerance,
#     Ls_seq = Ls_seq,
#     beta_equals_phi = FALSE,
#     verbose = FALSE)
# })
# 
# # FIXME: Large tolerance.
# test_that("H_di: Diffuse daily insolation on an inclined surface for beta = phi at VL2.", {
# 
#   # Tolerance.
#   tolerance = 0.12
# 
#   # Test input parameters.
#   Ls_seq = seq(0, 360, 5)
# 
#   # Expect equals all calculations against all expected.
#   test_daily_insolation_on_inclined_surface(
#     spacecraft = "VL2",
#     field = "Hd",
#     tolerance = tolerance,
#     Ls_seq = Ls_seq,
#     beta_equals_phi = TRUE,
#     verbose = FALSE)
# })
# 
# test_that("H_di: Diffuse daily insolation on optimal inclined surface for beta = 22° at VL2.", {
#   
#   # Tolerance.
#   tolerance = 0.06
#   
#   # Test input parameters.
#   Ls_seq = seq(0, 355, 5) # Areocentric longitude.
#   
#   # Expect equals all calculations against all expected.
#   test_daily_insolation_on_inclined_surface(
#     spacecraft = "VL2",
#     field = "Hd",
#     tolerance = tolerance,
#     Ls_seq = Ls_seq,
#     beta_equals_phi = FALSE,
#     verbose = FALSE)
# })
