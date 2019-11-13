
library(here)
library(testthat) 

Gh_eq = dget(here("functions", "G_h.R"))

test_that("I_h: Global irradiance on Mars horizontal surface with different normalized net flux functions.", {
  # Test parameters.
  Ls = 122    # Areocentric longitude.
  phi = 22.3  # Planetaryz latitude.
  al = 0.1    # Albedo.
  
  # Solar zenith angle.
  Z_seq = c(
    seq(0, 80, 10),
    85)
  
  # Optical depth.
  tau_seq = c(
    seq(0.1, 2.0, 0.1),
    seq(2.5, 3, 0.5),
    5, 6)
  
  for(Z in Z_seq){
    for(tau in tau_seq){
      
      # Calculate global irradiances.
      Gh_nfft_lookup_1 = Gh_eq(Ls=Ls, phi=phi, Z=Z, tau=tau, al=al, nfft=1)
      Gh_nfft_lookup_2 = Gh_eq(Ls=Ls, phi=phi, Z=Z, tau=tau, al=al, nfft=2)
      Gh_nfft_analytical = Gh_eq(Ls=Ls, phi=phi,  Z=Z, tau=tau, al=al, nfft=3)
      
      # Test assert equality.
      expect_equal(Gh_nfft_lookup_1, Gh_nfft_lookup_2, tolerance=4, scale=1)
      
      # For equality test against analytical approach, set the tolerance based on tau value.
      tolerance = 30
      
      if(tau == 6){
        tolerance = 205
        
      }else if(tau == 5){
        tolerance = 140
      }
      
      expect_equal(Gh_nfft_analytical, Gh_nfft_lookup_1, tolerance=tolerance, scale=1)
      expect_equal(Gh_nfft_analytical, Gh_nfft_lookup_2, tolerance=tolerance, scale=1)
    }
  }
})
