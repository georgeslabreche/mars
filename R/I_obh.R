# Equation 11 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param T_start 
#' @param T_end 
#' @param nfft 
#'
#' @return
#' @export
I_obh = function(Ls, phi, T_start, T_end, nfft){
  
  # Step 1: Constrain T_start and T_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls, phi, T_start, T_end)
  
  # No solar irradiance within the contrained time range.
  if(is.null(T_range)){
    return(0)
    
  }else{
    # Constrain the time range.
    T_start = T_range$T_start
    T_end = T_range$T_end
  }
  
  # Step 2: Calculate insolation.
  
  # The interand for Equation 11 (1990).
  interand = function(T_s){
    z = Z(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft)
    
    x = G_ob(Ls) * cos(z*pi/180)
    return(x)
  }
  
  # Equation 11 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
  Iobh = integrate(interand, T_start, T_end)
  
  return(Iobh$value)
}