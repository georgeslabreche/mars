# Equation 3 (1994): Global irradiance on Mars inclined surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994).
#   Solar radiation on Mars: Tracking photovoltaic array.
#   Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://ntrs.nasa.gov/?R=19950004977

# FIXME: Update this function so that it figures out if its a polar night or day.

# Equation 3 (1994): Global irradiance on an inclined surface.
#
#   Ls        - Areocentric longitude [deg].
#   omega     - Hour angle value [h].
#   phi       - Latitude [deg].
#   tau       - Optical depth.
#   al        - Albedo
#   beta      - Slope/Tilt angle [deg].
#   gamma_c   - Sun surface azimuth angle (i.e. orientation angle) [deg].
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param T_s 
#' @param z 
#' @param tau 
#' @param al 
#' @param beta 
#' @param gamma_c
#'
#' @return
#' @export
G_i = function(Ls, phi, longitude, T_s, z=Z(Ls=Ls, phi=phi, T_s=T_s), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180° and +180° with zero south, east negative, and west positive.")
  }
  
  a = G_bi(Ls=Ls, phi=phi, T_s=T_s, z=Z(Ls=Ls, phi=phi, T_s=T_s), tau=tau, beta=beta, gamma_c=gamma_c)
  b = G_di(Ls=Ls, phi=phi, longitude=longitude, T_s=T_s, z=Z(Ls=Ls, phi=phi, T_s=T_s), tau=tau, al=al, beta=beta)
  c = G_ali(Ls=Ls, phi=phi, longitude=longitude, T_s=T_s, z=Z(Ls=Ls, phi=phi, T_s=T_s), tau=tau, al=al, beta=beta)
  
  Gi = a + b + c
  
  return(Gi)
}

