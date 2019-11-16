

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
I_ali = function(Ls, phi, longitude, tau, Ts_start, Ts_end, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c){
  
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
  integrand = function(Ts){
    Gali = G_ali(Ls=Ls, phi=phi, longitude=longitude, Ts=Ts, tau=tau, al=al, beta=beta)
    return(Gali)
  }
  
  Iali = integrate(integrand, Ts_start, Ts_end)
  
  # Return integration result.
  return(Iali$value)
}
