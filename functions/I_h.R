# Global hourly insolation on Mars horizontal surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
library(here)

# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
Gh_eq = dget(here("functions", "G_h.R"))

# Constrain T_start and T_end based on sunrise and sunset times.
constrain_solar_time_range = dget(here("utils", "constrain_solar_time_range.R"))

# Albedo function.
source(here("functions", "albedo.R"))


function(Ls, phi, longitude, tau, T_start, T_end, al=albedo(latitude=phi, longitude=longitude, tau=tau), nfft){
  # Step 1: Constrain T_start and T_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls=Ls, phi=phi, T_start=T_start, T_end=T_end)
  
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
    G_h = Gh_eq(Ls=Ls, phi=phi, longitude=longitude, T_s=T_s, tau=tau, al=al, nfft=nfft)
    return(G_h)
  }
  
  # Global hourly insolation on Mars horizontal surface.
  I_h = integrate(interand, T_start, T_end)
  
  return(I_h$value)
}
