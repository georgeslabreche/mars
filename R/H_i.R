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
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param tau 
#' @param al 
#' @param beta 
#' @param gamma_c
#'
#' @return
#' @export
H_i = function(Ls, phi, longitude, tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180° and 180° with zero south, east negative, and west positive.")
  }
  
  # Hi is obtained by integrating Ii over the period from sunrise to sunset.
  Hi = I_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, Ts_start=0, Ts_end=24, al=al, beta=beta, gamma_c=gamma_c)
  
  # Return result.
  return(Hi)
}