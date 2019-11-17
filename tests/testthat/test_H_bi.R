context("Beam daily insolation on an inclined surface")
source("utils.R")

test_that("H_bi: Beam daily insolation on an inclined surface for beta = phi at VL1", {

  # Tolerance.
  tolerance = 0.03

  # Test input parameters.
  Ls_seq = seq(0, 0, 5)

  # Expect equals all calculations against all expected.
  test_daily_insolation_on_inclined_surface(
    spacecraft = "VL1",
    field = "Hbi",
    tolerance = tolerance,
    Ls_seq = seq(0, 355, 5),
    beta_equals_phi = TRUE,
    verbose = FALSE)
})

test_that("H_bi: Beam daily insolation on optimal inclined surface for beta = 6.5° at VL1", {
  
  # Tolerance.
  tolerance = 0.03
  
  # Test input parameters.
  Ls_seq = seq(0, 355, 5) # Areocentric longitude.
  
  # Expect equals all calculations against all expected.
  test_daily_insolation_on_inclined_surface(
    spacecraft = "VL1",
    field = "Hbi",
    tolerance = tolerance,
    Ls_seq = Ls_seq,
    beta_equals_phi = FALSE,
    verbose = FALSE)
})


test_that("H_bi: Beam daily insolation on an inclined surface for beta = phi at VL2", {

  # Tolerance.
  tolerance = 0.06

  # Test input parameters.
  Ls_seq = seq(0, 360, 5)

  # Expect equals all calculations against all expected.
  test_daily_insolation_on_inclined_surface(
    spacecraft = "VL2",
    field = "Hbi",
    tolerance = tolerance,
    Ls_seq = Ls_seq,
    beta_equals_phi = TRUE,
    verbose = FALSE)
})

test_that("H_bi: Beam daily insolation on optimal inclined surface for beta = 22° at VL2", {

  # Tolerance.
  tolerance = 0.08

  # Test input parameters.
  Ls_seq = seq(0, 355, 5) # Areocentric longitude.

  # Expect equals all calculations against all expected.
  test_daily_insolation_on_inclined_surface(
    spacecraft = "VL2",
    field = "Hbi",
    tolerance = tolerance,
    Ls_seq = Ls_seq,
    beta_equals_phi = FALSE,
    verbose = FALSE)
})
