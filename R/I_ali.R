

#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param tau 
#' @param T_start 
#' @param T_end 
#' @param al 
#' @param beta 
#' @param gamma_c
#'
#' @return
#' @export
I_ali = function(Ls, phi, longitude, tau, T_start, T_end, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c){
  
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
  interand = function(T_s){
    Gali = G_ali(Ls=Ls, phi=phi, longitude=longitude, T_s=T_s, tau=tau, al=al, beta=beta)
    return(Gali)
  }
  
  Iali = integrate(interand, T_start, T_end)
  
  # Return integration result.
  return(Iali$value)
}
