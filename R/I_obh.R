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
#' FIXME: Remove nfft. Remove in all I functions.
I_obh = function(Ls, phi, T_start, T_end, nfft){
  
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
  
  # The integrand for Equation 11 (1990).
  integrand = function(T_s){
    z = Z(Ls=Ls, T_s=T_s, phi=phi)

    x = G_ob(Ls) * cos(z*pi/180)
    return(x)
  }

  # Equation 11 (1990): Beam insolation on a horizontal surface at the top of Mars atmosphere [Wh/m2].
  Iobh = integrate(integrand, T_start, T_end)

  return(Iobh$value)
  
  # delta = declination(Ls)
  # 
  # # Hour angles [deg].
  # omega_start = 15 * T_start - 180
  # omega_end = 15 * T_end - 180
  # 
  # a = (2 * pi * (omega_end - omega_start)) / 360
  # b = sin(phi*pi/180) * sin(delta)
  # c = cos(phi*pi/180) * sin(delta) * (sin(omega_end*pi/180) - sin(omega_start*pi/180))
  # 
  # Iobh = 12/pi * G_ob(Ls) * (a * b + c) 
  # 
  # return(Iobh)
  
}
