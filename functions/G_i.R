# Equation 3 (1994): Global irradiance on Mars inclined surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994).
#   Solar radiation on Mars: Tracking photovoltaic array.
#   Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://ntrs.nasa.gov/?R=19950004977

# FIXME: Update this function so that it figures out if its a polar night or day.

library(here)

# Equation 6 (1990): Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

Gbi_eq = dget(here("functions", "G_bi.R"))
Gdi_eq = dget(here("functions", "G_di.R"))
Gali_eq = dget(here("functions", "G_ali.R"))#

source(here("functions", "albedo.R"))

# Equation 3 (1994): Global irradiance on an inclined surface.
#
#   Ls        - Areocentric longitude [deg].
#   omega     - Hour angle value [h].
#                 - An integer belonging to [6, 18] if using net flux function f_89 or f_90 (nfft 1 or 2).
#                 - An interger belonging to [0, 24] if using net flux function f_analytical (nfft 3).
#   phi       - Latitude [deg].
#   tau       - Optical depth.
#   al        - Albedo
#   beta      - Slope/Tilt angle [deg].
#   gamma_c   - Sun surface azimuth angle (i.e. orientation angle) [deg].
#   nfft      - Net flux function implementation type.
#                 - 1 for f_89.
#                 - 2 for f_90.
#                 - 3 for f_analytical.
function(Ls, phi, longitude, T_s, Z=Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c, nfft){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
  }
  
  a = Gbi_eq(Ls=Ls, phi=phi, T_s=T_s, Z=Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft), tau=tau, beta=beta, gamma_c=gamma_c, nfft=nfft)
  b = Gdi_eq(Ls=Ls, phi=phi, longitude=longitude, T_s=T_s, Z=Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft), tau=tau, al=al, beta=beta, nfft=nfft)
  c = Gali_eq(Ls=Ls, phi=phi, longitude=longitude, T_s=T_s, Z=Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft), tau=tau, al=al, beta=beta, nfft=nfft)
  
  Gi = a + b + c
  
  return(Gi)
}

