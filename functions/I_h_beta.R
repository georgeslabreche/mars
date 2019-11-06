# Global hourly insolation on Mars inclined surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994).
#   Solar radiation on Mars: Tracking photovoltaic array.
#   Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://www.researchgate.net/publication/24286713_Solar_radiation_on_Mars_Tracking_photovoltaic_array
#
library(here)

# Equation 3 (1994): Global irradiance on Mars inclined surface [W/m2].
Gh_beta_eq = dget(here("functions", "G_h_beta.R"))

# Constrain T_start and T_end based on sunrise and sunset times.
constrain_solar_time_range = dget(here("utils", "constrain_solar_time_range.R")) 

function(Ls, phi, tau, T_start, T_end, al=0.1, beta, gamma_c, nfft){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
  }
  
  # Step 1: Constrain T_start and T_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls, phi, T_start, T_end, beta, gamma_c)
  
  # No solar irradiance within the contrained time range.
  if(is.null(T_range)){
    return(0)
    
  }else{
    # Constrain the time range.
    T_start = T_range$T_start
    T_end = T_range$T_end
  }
  
  # Step 2: Calculate insolation.
  
  # The interand for Equation 19 (1990).
  interand = function(T_s){
    G_h_beta = Gh_beta_eq(Ls=Ls, phi=phi, T_s=T_s, tau=tau, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
    return(G_h_beta)
  }
  
  # Global hourly insolation on Mars horizontal surface.
  I_h_beta = integrate(interand, T_start, T_end)
  
  # Return integration result.
  return(I_h_beta$value)
}
