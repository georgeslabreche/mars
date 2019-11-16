#' Title
#'
#' @param Ls 
#' @param phi 
#' @param tau 
#' @param T_start 
#' @param T_end 
#' @param beta 
#' @param gamma_c
#'
#' @return
#' @export
I_bi = function(Ls, phi, tau, T_start, T_end, beta, gamma_c){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180° and +180° with zero south, east negative, and west positive.")
  }
  
  # Step 1: Constrain T_start and T_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls=Ls, phi=phi, T_start=T_start, T_end=T_end, beta=beta, gamma_c=gamma_c)
  
  # No solar irradiance within the contrained time range.
  if(is.null(T_range)){
    return(0)
    
  }else{
    # Constrain the time range.
    T_start = T_range$T_start
    T_end = T_range$T_end
  }
  
  # Step 2: Calculate insolation.
  
  integrand = function(T_s){
    Gbi = G_bi(Ls=Ls, phi=phi, T_s=T_s, tau=tau, beta=beta, gamma_c=gamma_c)
    return(Gbi)
  }
  
  Ibi = integrate(integrand, T_start, T_end)
  
  # Return integration result.
  return(Ibi$value)
}
