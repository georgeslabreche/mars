# Beam daily insolation on Mars horizontal surface [Wh/m2-day].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE V. - HOURLY AND DAILY BEAM INSOLATION ON A HORIZONTAL SURFACE AT MARS SURFACE.
#   TABLE V. Daily insolation on a horizontal surface (Update 1990).

context("Beam daily insolation on a horizontal surface")
source("utils.R")

#Test with expected results from TABLE V.
test_that("H_bh: Beam daily insolation on a horizontal surface", {
  # 41 Wh/m2 error tolerance.
  tolerance = 41

  phi = spacecrafts$VL1$latitude

  expected_results = list(
    "69" = c(0.65, 1768), # FIXME: Larger error (40).
    "120" = c(0.40, 2534),
    "153" = c(0.50, 2308),
    "249" = c(1.40, 314),
    "299" = c(3.25, 12))

  index = 1
  for(expected_result in expected_results){

    Ls = strtoi(names(expected_results)[index])
    tau = expected_result[1]
    Hbh_expected = expected_result[2]

    Hbh = H_bh(Ls=Ls, phi=phi, tau=tau)
    expect_equal(Hbh, Hbh_expected, tolerance=tolerance, scale=1)

    index = index + 1
  }
})

test_that("H_bh: Beam daily insolation on a horizontal surface at VL1", {
  # 3% Error tolerance.
  tol = 0.03

  # Test input parameter.
  Ls_seq = seq(0, 355, 5)

  # Run the tests.
  test_daily_insolation_on_horizontal_surface(
    spacecraft = "VL1",
    field = "Hbh",
    tolerance = tol,
    Ls_seq = Ls_seq,
    verbose = FALSE)
})

test_that("H_bh: Beam daily insolation on a horizontal surface at VL2", {
  # 4% Error tolerance.
  tol = 0.04
  
  # Test input parameter.
  Ls_seq = seq(0, 355, 5)
  
  # Run the tests.
  test_daily_insolation_on_horizontal_surface(
    spacecraft = "VL2",
    field = "Hbh",
    tolerance = tol,
    Ls_seq = Ls_seq,
    verbose = FALSE)
})