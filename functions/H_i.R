# Global daily insolation on Mars inclined surface [Wh/m2-day].
#
# Obtained by integrating I_h_beta, global hourly insolation on Mars inclined surface, 
# over the period from sunrise to sunset.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994).
#   Solar radiation on Mars: Tracking photovoltaic array.
#   Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://ntrs.nasa.gov/?R=19950004977
#

library(here)

# Global hourly insolation on Mars inclined surface [W/m2-h].
Ii_eq = dget(here("functions", "I_i.R"))

source(here("functions", "albedo.R"))

function(Ls, phi, longitude, tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c, nfft){
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degrees with zero south, east negative, and west positive.")
  }
  
  # H_i is obtained by integrating I_i over the period from sunrise to sunset.
  H_i = Ii_eq(Ls=Ls, phi=phi, longitude=longitude, tau=tau, T_start=0, T_end=24, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
  
  # Return result.
  return(H_i)
}