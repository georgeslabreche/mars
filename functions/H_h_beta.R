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
Ih_beta_eq = dget(here("functions", "I_h_beta.R"))

#Hh_beta_eq = function(Ls, phi, tau, al=0.1, beta, gamma_c, nfft){
function(Ls, phi, tau, al=0.1, beta, gamma_c, nfft){
  # H_h_beta is obtained by integrating I_h_beta over the period from sunrise to sunset.
  H_h_beta = Ih_beta_eq(Ls=Ls, phi=phi, tau=tau, T_start=0, T_end=24, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
  return(H_h_beta)
}

#H_h_beta = Hh_beta_eq(Ls=200, phi=22.5, tau=0.5, al=0.1, beta=30.59082, gamma_c=0, nfft=3)
#print(H_h_beta)