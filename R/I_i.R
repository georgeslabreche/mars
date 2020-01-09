# Global hourly insolation on Mars inclined surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994).
#   Solar radiation on Mars: Tracking photovoltaic array.
#   Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://www.researchgate.net/publication/24286713_Solar_radiation_on_Mars_Tracking_photovoltaic_array
#

#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param tau 
#' @param Ts_start 
#' @param Ts_end 
#' @param al 
#' @param beta 
#' @param gamma_c
#'
#' @return
#' @export
I_i = function(Ls, phi, longitude, tau, Ts_start, Ts_end, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180° and +180° with zero south, east negative, and west positive.")
  }
  
  # Step 1: Constrain Ts_start and Ts_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls=Ls, phi=phi, Ts_start=Ts_start, Ts_end=Ts_end, beta=beta, gamma_c=gamma_c)
  
  # No solar irradiance within the contrained time range.
  if(is.null(T_range)){
    return(0)
    
  }else{
    # Constrain the time range.
    Ts_start = T_range$Ts_start
    Ts_end = T_range$Ts_end
  }
  
  # Step 2: Calculate insolation.
  
  # The integrand for Equation 19 (1990).
  integrand = function(Ts){
    Gi = G_i(Ls=Ls, phi=phi, longitude=longitude, Ts=Ts, tau=tau, al=al, beta=beta, gamma_c=gamma_c)
    return(Gi)
  }
  
  # Global hourly insolation on Mars inclined surface.
  Ii = integrate(integrand, Ts_start, Ts_end)
  
  # Return integration result.
  return(Ii$value)
}