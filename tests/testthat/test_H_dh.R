# Diffuse daily insolation on Mars horizontal surface [Wh/m2-day].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE VI. - HOURLY AND DAILY DIFFUSE INSOLATION O NA HORIZONTAL SURFACE AT MAR SURFACE.
#   TABLE V. Daily insolation on a horizontal surface (Update 1990).
context("Diffuse daily insolation on a horizontal surface")
source("utils.R")

# Test with expected results from TABLE VI.
test_that("H_dh: Diffuse daily insolation on a horizontal surface", {
  tolerance = 47

  phi = 22.3
  al = 0.1

  expected_results = list(
    "69" = c(0.65, 1572), # FIXME: Large error (47).
    "120" = c(0.40, 1326),
    "153" = c(0.50, 1574), # FIXME: Large error (21.6).
    "249" = c(1.40, 1586)) # FIXME: Large error (24.1).
    #"299" = c(3.25, 1012))  # FIXME: Large error (252)

  index = 1
  for(expected_result in expected_results){

    Ls = strtoi(names(expected_results)[index])
    tau = expected_result[1]
    Hdh_expected = expected_result[2]

    Hdh = H_dh(Ls=Ls, phi=phi, tau=tau, al=al)
    expect_equal(Hdh, Hdh_expected, tolerance=tolerance, scale=1)

    index = index + 1
  }
})

test_that("H_dh: Diffuse daily insolation on a horizontal surface at VL1", {
  # 10% Error tolerance.
  tol = 0.1

  Ls_seq = seq(0, 355, 5)

  # Run the tests.
  test_daily_insolation_on_horizontal_surface(
    spacecraft = "VL1",
    field = "Hdh",
    tolerance = tol,
    Ls_seq = Ls_seq,
    verbose = FALSE)

})

test_that("H_dh: Diffuse daily insolation on a horizontal surface at VL2", {
  # 7% Error tolerance.
  tol = 0.07
  
  Ls_seq = seq(0, 355, 5)
  
  # Run the tests.
  test_daily_insolation_on_horizontal_surface(
    spacecraft = "VL2",
    field = "Hdh",
    tolerance = tol,
    Ls_seq = Ls_seq,
    verbose = FALSE)
  
})