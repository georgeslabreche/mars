# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface [W/m2-hr].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE V. - HOURLY AND DAILY BEAM INSOLATION ON A HORIZONTAL SURFACE AT MARS SURFACE.

library(testthat) 
library(here)

Ibh_eq = dget(here("functions", "I_bh.R"))
tolerance = 10

test_that("Equation 19 (1990): I_bh. For VL1 at Ls=69° and τ=0.65.", {

  expected_at_Ls69 = c(252, 230, 186, 128, 67, 20, 3)
  index = 1
  for(T_start in 12:18){
    Ibh = Ibh_eq(Ls=69, phi=22.3, tau=0.65, T_start=T_start, T_end=T_start+1)
    Ibh_expected = expected_at_Ls69[index]

    expect_equal(Ibh, Ibh_expected, tolerance=tolerance, scale=1)

    index = index + 1
  }
})

test_that("Equation 19 (1990): I_bh. For VL1 at Ls=120° and τ=0.40.", {

  expected_at_Ls120 = c(352, 322, 265, 190, 103, 33, 2)
  index = 1
  for(T_start in 12:18){
    Ibh = Ibh_eq(Ls=129, phi=22.3, tau=0.40, T_start=T_start, T_end=T_start+1)
    Ibh_expected = expected_at_Ls120[index]

    expect_equal(Ibh, Ibh_expected, tolerance=tolerance, scale=1)

    index = index + 1
  }
})

test_that("Equation 19 (1990): I_bh. For VL1 at Ls=153° and τ=0.50.", {

  expected_at_Ls153 = c(345, 310, 244, 163, 77, 15, 0)
  index = 1
  for(T_start in 12:18){
    Ibh = Ibh_eq(Ls=153, phi=22.3, tau=0.50, T_start=T_start, T_end=T_start+1)
    Ibh_expected = expected_at_Ls153[index]

    expect_equal(Ibh, Ibh_expected, tolerance=tolerance, scale=1)

    index = index + 1
  }
})

test_that("Equation 19 (1990): I_bh. For VL1 at Ls=249° and τ=1.40.", {   
  
  expected_at_Ls249 = c(69, 50, 26, 10, 2, 0, 0)
  index = 1
  for(T_start in 12:18){
    Ibh = Ibh_eq(Ls=249, phi=22.3, tau=1.40, T_start=T_start, T_end=T_start+1)
    Ibh_expected = expected_at_Ls249[index]

    expect_equal(Ibh, Ibh_expected, tolerance=tolerance, scale=1)

    index = index + 1
  }
})

test_that("Equation 19 (1990): I_bh. For VL1 at Ls=299° and τ=3.25.", {

  expected_at_Ls299 = c(3, 2, 1, 0, 0, 0, 0)
  index = 1
  for(T_start in 12:18){
    Ibh = Ibh_eq(Ls=299, phi=22.3, tau=3.25, T_start=T_start, T_end=T_start+1)
    Ibh_expected = expected_at_Ls299[index]

    expect_equal(Ibh, Ibh_expected, tolerance=tolerance, scale=1)

    index = index + 1
  }
})


