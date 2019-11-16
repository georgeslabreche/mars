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
#' @param Ts_start 
#' @param Ts_end 
#' @param al
#'
#' @return
#' @export
I_h = function(Ls, phi, longitude, tau, Ts_start, Ts_end, al=albedo(latitude=phi, longitude=longitude, tau=tau)){
  
  # Step 1: Constrain Ts_start and Ts_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls=Ls, phi=phi, Ts_start=Ts_start, Ts_end=Ts_end)
  
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
    Gh = G_h(Ls=Ls, phi=phi, longitude=longitude, Ts=Ts, tau=tau, al=al)
    return(Gh)
  }
  
  # Global hourly insolation on Mars horizontal surface.
  Ih = integrate(integrand, Ts_start, Ts_end)
  
  return(Ih$value)
}
