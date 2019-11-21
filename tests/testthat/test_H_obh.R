# Equation 13 (1990): Daily beam insolation on a horizontal surface at top of Mars atmosphere [Wh/m2-day].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE II. - HOURLY AND DAILY BEAM INSOLATION ON A HORIZONTAL SURFACE AT TOP OF MARS ATMOSPHERE
context("Daily beam insolation on a horizontal surface at top of Mars atmosphere")


test_that("H_obh: Integration yields same results as equivalent formula.",{
  
  # Tolerance.
  tolerance = 1e-10
  
  # Formula that is equivalent to integration I_obh from sunrise to sunset.
  H_obh_eq = function(Ls, phi){
    
    # Equation 7 (1990): Declination angle [rad].
    delta = declination(Ls)
    
    # Equation 9 (1990): The sunset hour angle [rad].
    omega_ss = sunset(Ls=Ls, phi=phi, unit=1)
    
    # Equation 13 (1990): Daily beam insolation on a horizontal surfce at top of Mars atmosphere [Wh/m2-day].
    a = (24/pi) * G_ob(Ls)
    b = 2 * pi * (omega_ss*180/pi) / 360
    c = sin(phi*pi/180) * sin(delta)
    d = cos(phi*pi/180) * cos(delta) * sin(omega_ss)
    
    Hobh = a * (b * c + d)
    
    # Return result.
    return(Hobh)    
  }
  
  # Test.
  for(Ls in seq(0, 360, 10)){
    for(phi in seq(-60, 60, 10)){
      # Calculate insolations.
      Hobh = H_obh(Ls=Ls, phi=phi)
      Hobh_eq = H_obh_eq(Ls=Ls, phi=phi)
      
      # Compare them.
      expect_equal(Hobh, Hobh_eq, tolerance=tolerance, scale=1)
    }
  }
})

# Test with expected results from TABLE II.
test_that("Equation 13 (1990): H_obh.", {

  # Tolerance.
  tolerance = 6

  # Constant test input parameters.
  phi = 22.3

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