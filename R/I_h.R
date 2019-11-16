# Global hourly insolation on Mars horizontal surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#



#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param tau 
#' @param T_start 
#' @param T_end 
#' @param al
#'
#' @return
#' @export
I_h = function(Ls, phi, longitude, tau, T_start, T_end, al=albedo(latitude=phi, longitude=longitude, tau=tau)){
  
  # Step 1: Constrain T_start and T_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls=Ls, phi=phi, T_start=T_start, T_end=T_end)
  
  # No solar irradiance within the contrained time range.
  if(is.null(T_range)){
    return(0)
    
  }else{
    # Constrain the time range.
    T_start = T_range$T_start
    T_end = T_range$T_end
  }
  
  # Step 2: Calculate insolation.
  
  # The integrand for Equation 19 (1990).
  integrand = function(T_s){
    Gh = G_h(Ls=Ls, phi=phi, longitude=longitude, T_s=T_s, tau=tau, al=al)
    return(Gh)
  }
  
  # Global hourly insolation on Mars horizontal surface.
  Ih = integrate(integrand, T_start, T_end)
  
  return(Ih$value)
}
