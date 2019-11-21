context("Normalized net flux function")

# Assert equals test function.
assert = function(x, al){
  net_flux = f(z=x["z"], tau=x["tau"], al=al)
  expect_equal(unname(x["netflux"]), net_flux, tolerance=0, scale=1)
}

test_that("f function: toggling warnings.", {
  Sys.setenv(NET_FLUX_FUNCTION_TYPE = "polynomial")
  
  # Enable warnings
  Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = TRUE)
  
  # Trigger warning regarding tau greater than 5.
  expect_warning(f(z=10, tau=6, al=0.1))
  
  # Trigger warning regarding z greater or equal than 80.
  expect_warning(f(z=85, tau=0.5, al=0.1))
  
  for(false_flag in c("1", "f", "false", "n", "no", "FaLsE", "FALSE", "F", "No", "NO", "N", FALSE)){
    # Disable warnings
    Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = false_flag)
    
    # Ignore warning regarding tau greater than 5.
    expect_silent(f(z=10, tau=6, al=0.1))
    
    # Ignore warning regarding z greater or equal than 80.
    expect_silent(f(z=85, tau=0.5, al=0.1))
  }
  
  # Enable warnings by setting show warnings flag to unrecognized values
  Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = "asdasd")
  
  # Trigger warning regarding tau greater than 5.
  expect_warning(f(z=10, tau=6, al=0.1))
  
  # Trigger warning regarding z greater or equal than 80.
  expect_warning(f(z=85, tau=0.5, al=0.1))
  

})

test_that("f function: comparing normalized flux lookup table values.", {
  # Test parameters.
  Z_seq = c(seq(0, 80, 10), 85)

  tau_seq = c(
    seq(0.1, 2.0, 0.1),
    seq(2.5, 3, 0.5),
    5, 6)

  al = 0.1 # Albedo.

  for(z in Z_seq){
    for(tau in tau_seq){

      # Calculate global irradiances.
      Sys.setenv(NET_FLUX_FUNCTION_TYPE = "lookup_v1")
      net_flux_lookup_v1 = f(z=z, tau=tau, al=al)

      Sys.setenv(NET_FLUX_FUNCTION_TYPE = "lookup_v2")
      net_flux_lookup_v2 = f(z=z, tau=tau, al=al)

      # Test assert equality.
      # Normalized net flux values in lookup tables lookup_v1 and lookup_v2 are not exactly equal all the time
      # but they are at least approximately equal.
      expect_equal(net_flux_lookup_v1, net_flux_lookup_v2, tolerance=0.051, scale=1)
    }
  }
})

test_that("f function - lookup_v2: albedo 0.1.", {

  # Expected results.
  expected_results = data.frame(
    "z" =       c(   0,  10,   15,   20,    25,   30,   35,  40,    45,   50),
    "tau" =     c(0.15, 0.55, 1.25, 1.75, 2.20, 3.00, 3.60, 4.50, 5.00, 6.00),
    "netflux" = c(.875, .802, .678, .592, .518, .411, .337, .257, .212, .154))

  # Apply test function.
  Sys.setenv(NET_FLUX_FUNCTION_TYPE = "lookup_v2")
  apply(expected_results, 1, assert, al=0.1)
})


test_that("f function - lookup_v2: albedo 0.4.", {

  # Expected results.
  expected_results = data.frame(
    "z" =       c(  40,   45,   50,    55,  60,   65,   70,    75,   80,   85),
    "tau" =     c(0.15, 0.55, 1.25, 1.75, 2.20, 3.00, 3.60, 4.50, 5.00, 6.00),
    "netflux" = c(.582, .519, .404, .324, .256, .182, .137, .099, .078, .054))


  # Apply test function.
  Sys.setenv(NET_FLUX_FUNCTION_TYPE = "lookup_v2")
  apply(expected_results, 1, assert, al=0.4)
})

test_that("f function - polynomial: albedo 0.1.", {

  # Test input parameters.
  z_seq = seq(0, 85, 5)

  tau_seq = c(
    seq(0.1, 2, 0.05)
    # TODO: seq(2.1, 3, 0.1),
    # TODO: seq(3.2, 4, 0.2)
    # TODO: seq(4.5, 6, 0.5)
  )

  al = 0.1 # Albedo

  # Analytical function should return net flux that is approximately equal to those found in the lookup tables
  for(z in z_seq){
    for(tau in tau_seq){

      Sys.setenv(NET_FLUX_FUNCTION_TYPE = "lookup_v2")
      net_flux_lookup = f(z=z, tau=tau, al=al)

      Sys.setenv(NET_FLUX_FUNCTION_TYPE = "polynomial")
      net_flux_polynomial = f(z=z, tau=tau, al=al)

      # Larger divergences for high values of Z.
      # Maximum error is 7% for Z = 80° or Z = 85°.
      # Adjust acceptale tolerance accordingly.
      tolerance = ifelse(z >= 65, 0.0471, 0.0171)

      expect_equal(net_flux_lookup, net_flux_polynomial, tolerance=tolerance, scale=1)
    }
  }
})


test_that("f function - polynomial: albedo 0.4.", {

  # Test input parameters.
  z_seq = seq(0, 85, 5)

  tau_seq = c(
    seq(0.1, 2, 0.05)
    # TODO: seq(2.1, 3, 0.1),
    # TODO: seq(3.2, 4, 0.2)
    # TODO: seq(4.5, 6, 0.5)
  )

  al = 0.4 # Albedo

  # Analytical function should return net flux that is approximately equal to those found in the lookup tables
  for(z in z_seq){
    for(tau in tau_seq){
      Sys.setenv(NET_FLUX_FUNCTION_TYPE = "lookup_v2")
      net_flux_lookup = f(z=z, tau=tau, al=al)

      Sys.setenv(NET_FLUX_FUNCTION_TYPE = "polynomial")
      net_flux_polynomial = f(z=z, tau=tau, al=al)

      # Larger divergences for high values of z.
      # Maximum error is 7% for z = 80° or z = 85°.
      # Adjust acceptale tolerance accordingly.
      tolerance = ifelse(z >= 65, 0.0471, 0.0171)

      expect_equal(net_flux_lookup, net_flux_polynomial, tolerance=tolerance, scale=1)
    }
  }
})
