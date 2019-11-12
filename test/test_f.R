
library(testthat) 
library(here)

f = dget(here("functions", "f.R"))

# Assert equals test function.
assert = function(x, al, pub_year){
  net_flux = f(Z=x["Z"], tau=x["tau"], al=al, pub_year=pub_year)
  expect_equal(unname(x["netflux"]), net_flux, tolerance=0, scale=1)
}

test_that("f function - 1990 Update: albedo 0.1.", {

  # Expected results.
  expected_results = data.frame(
    "Z" =       c(   0,  10,   15,   20,    25,   30,   35,  40,    45,   50),
    "tau" =     c(0.15, 0.55, 1.25, 1.75, 2.20, 3.00, 3.60, 4.50, 5.00, 6.00),
    "netflux" = c(.875, .802, .678, .592, .518, .411, .337, .257, .212, .154))

  # Apply test function.
  apply(expected_results, 1, assert, al=0.1, pub_year=1990)
})


test_that("f function - 1990 Update: albedo 0.4.", {

  # Expected results.
  expected_results = data.frame(
    "Z" =       c(  40,   45,   50,    55,  60,   65,   70,    75,   80,   85),
    "tau" =     c(0.15, 0.55, 1.25, 1.75, 2.20, 3.00, 3.60, 4.50, 5.00, 6.00),
    "netflux" = c(.582, .519, .404, .324, .256, .182, .137, .099, .078, .054))


  # Apply test function.
  apply(expected_results, 1, assert, al=0.4, pub_year=1990)
})

test_that("f function - analytical: albedo 0.1.", {
  
  # Test input parameters.
  Z_seq = seq(0, 85, 5)
  
  tau_seq = c(
    seq(0.1, 2, 0.05)
  )
  
  al = 0.1 # Albedo
  
  # Analytical function should return net flux that is approximately equal to those found in the lookup tables
  for(Z in Z_seq){
    for(tau in tau_seq){
      net_flux_lookup = f(Z=Z, tau=tau, al=al, pub_year=1990)
      net_flux_analytical = f(Z=Z, tau=tau, al=al)
      
      # Larger divergences for high values of Z.
      # Maximum error is 7% for Z = 80° or Z = 85°.
      # Adjust acceptale tolerance accordingly.
      tolerance = ifelse(Z >= 65, 0.0471, 0.0171)
      
      expect_equal(net_flux_lookup, net_flux_analytical, tolerance=tolerance, scale=1)
    }
  }
})


test_that("f function - analytical: albedo 0.4.", {
  
  # Test input parameters.
  Z_seq = seq(0, 85, 5)
  
  tau_seq = c(
    seq(0.1, 2, 0.05)
  )
  
  al = 0.4 # Albedo
  
  # Analytical function should return net flux that is approximately equal to those found in the lookup tables
  for(Z in Z_seq){
    for(tau in tau_seq){
      net_flux_lookup = f(Z=Z, tau=tau, al=al, pub_year=1990)
      net_flux_analytical = f(Z=Z, tau=tau, al=al)
      
      # Larger divergences for high values of Z.
      # Maximum error is 7% for Z = 80° or Z = 85°.
      # Adjust acceptale tolerance accordingly.
      tolerance = ifelse(Z >= 65, 0.0471, 0.0171)
      
      expect_equal(net_flux_lookup, net_flux_analytical, tolerance=tolerance, scale=1)
    }
  }
})
