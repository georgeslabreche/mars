# Equation 12 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# Expected results taken from:
#   TABLE II. - HOURLY AND DAILY BEAM IN SOLATION ON A HORIZONTAL SURFACE AT TOP OF MARS ATMOSPHERE

context("Beam insolation on a horizotal surface at the top of Mars atmosphere")

test_that("I_obh: Integration yields same results as equivalent formulas.",{
  
  # Tolerance.
  tolerance = 1e-10
  
  # Equation 11 in 1989.
  I_obh_eq1 = function(Ls, phi, Ts_start, Ts_end){
    
    # Declination.
    delta = declination(Ls)
    
    integrand = function(omega){
      i = sin(phi*pi/180) * sin(delta) + cos(phi*pi/180) * cos(delta) * cos(omega)
      return(i)
    }
    
    omega_start_rad = (15 * Ts_start - 180)*pi/180
    omega_end_rad = (15 * Ts_end - 180)*pi/180
    
    x =  integrate(integrand, omega_start_rad, omega_end_rad)
    Iobh = 12/pi * G_ob(Ls) * x$value
    
    return(Iobh)
  }
  
  # Equation 12 in 1989.
  I_obh_eq2 = function(Ls, phi, Ts_start, Ts_end){
    
    # Declination.
    delta = declination(Ls)

    # Hour angles [deg].
    omega_start = 15 * Ts_start - 180
    omega_end = 15 * Ts_end - 180

    a = (2 * pi * (omega_end - omega_start)) / 360
    b = sin(phi*pi/180) * sin(delta)
    c = cos(phi*pi/180) * cos(delta) * (sin(omega_end*pi/180) - sin(omega_start*pi/180))

    Iobh = 12/pi * G_ob(Ls) * (a * b + c)
    
    return(Iobh)
  }
  
  # Test.
  # for(Ls in seq(0, 360, 20)){
  #   for(phi in seq(-60, 60, 20)){
  #     for(Ts_start in 12:18){
  for(Ls in 100){
    for(phi in 30){
      for(Ts_start in 13){
        # Calculate insolations.
        Iobh = I_obh(Ls=Ls, phi=phi, Ts_start=Ts_start, Ts_end=Ts_start+1)
        Iobh_eq1 = I_obh_eq1(Ls=Ls, phi=phi, Ts_start=Ts_start, Ts_end=Ts_start+1)
        Iobh_eq2 = I_obh_eq2(Ls=Ls, phi=phi, Ts_start=Ts_start, Ts_end=Ts_start+1)

        # Compare them.
        expect_equal(Iobh_eq1, Iobh_eq2, tolerance=tolerance, scale=1)
      }
    }
  }
})

test_that("I_obh.", {

  # Tolerance
  tolerance = 9

  # Test input parameters
  phi = 22.3

  # Expected hourly I_obh for different areocentric longitudes.
  # Test for the 1 hour time ranges at 12-13, 13-14, 14-15, 15-16, 16-17, 17-18, and 18-19.
  expected_results = list(
    "69" = c(488, 460, 405, 328, 234, 128, 25),
    "120" = c(528, 497, 437, 353, 249, 134, 23),
    "153" = c(572, 536, 467, 368, 247, 113, 7),
    "249" = c(496, 455, 376, 263, 126, 8, 0),
    "299" = c(478, 439, 364, 257, 127, 19, 0))


  # Test.
  expected_result_index = 1
  for(expected_result in expected_results){
    Ls = strtoi(names(expected_results[expected_result_index]))

    hour_index = 1
    for(Ts_start in 12:18){
      Iobh = I_obh(Ls=Ls, phi=phi, Ts_start=Ts_start, Ts_end=Ts_start+1)
      Iobh_expected = expected_result[hour_index]

      expect_equal(Iobh, Iobh_expected, tolerance=tolerance, scale=1)

      hour_index = hour_index + 1
    }

    expected_result_index = expected_result_index + 1
  }
})