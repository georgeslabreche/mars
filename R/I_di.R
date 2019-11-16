

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
I_di = function(Ls, phi, longitude, tau, Ts_start, Ts_end, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180° and +180° with zero south, east negative, and west positive.")
  }
  
  # Step 1: Constrain Ts_start and Ts_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls, phi, Ts_start, Ts_end, beta, gamma_c)
  
  # No solar irradiance within the contrained time range.
  if(is.null(T_range)){
    return(0)
    
  }else{
    # Constrain the time range.
    Ts_start = T_range$Ts_start
    Ts_end = T_range$Ts_end
  }
  
  # Step 2: Calculate insolation.

  integrand = function(Ts){
    Gdi = G_di(Ls=Ls, phi=phi, longitude=longitude, Ts=Ts, tau=tau, al=al, beta=beta)
    return(Gdi)
  }
  
  Idi = integrate(integrand, Ts_start, Ts_end)
  
  # Return integration result.
  return(Idi$value)
}
