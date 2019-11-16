# Equation 13 (1990): Daily beam insolation on a horizontal surfce at top of Mars atmosphere [Wh/m2-day].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE II. - HOURLY AND DAILY BEAM INSOLATION ON A HORIZONTAL SURFACE AT TOP OF MARS ATMOSPHERE
context("Daily beam insolation on a horizontal surfce at top of Mars atmosphere")
phi = 22.3

# Test with expected results from TABLE II.
test_that("Equation 13 (1990): H_obh.", {

  tolerance = 6

  # Expected results: Ls and H_obh.
  expected_results = list(
    "69" = 4136,
    "120" = 4442,
    "153" = 4620,
    "249" = 3449,
    "299" = 3350)

  index = 1
  for(Hobh_expected in expected_results){
    Ls = strtoi(names(expected_results)[index])
    Hobh = H_obh(Ls, phi)

    expect_equal(Hobh, Hobh_expected, tolerance=tolerance, scale=1)

    index = index + 1
  }
})

# H_obh is obtained by integrating I_obh over the period from sunrise to sunset.
# So we can also test if this equality is true.
test_that("Equation 13 (1990): H_obh (compared with I_obh from sunrise to sunset).", {
  tolerance = 1e-10

  index = 1
  for(Ls in 0:360){
    Hobh = H_obh(Ls=Ls, phi=phi)
    Iobh_day = I_obh(Ls=Ls, phi=phi, T_start=0, T_end=24)

    expect_equal(Hobh, Iobh_day, tolerance=tolerance, scale=1)

    index = index + 1
  }
})