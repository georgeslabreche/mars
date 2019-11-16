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
#' @param Ts_start 
#' @param Ts_end
#'
#' @return
#' @export
I_obh = function(Ls, phi, Ts_start, Ts_end){
  
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
  
  # The integrand for Equation 11 (1990).
  integrand = function(Ts){
    z = Z(Ls=Ls, phi=phi, Ts=Ts)

    x = G_ob(Ls) * cos(z*pi/180)
    return(x)
  }

  # Equation 11 (1990): Beam insolation on a horizontal surface at the top of Mars atmosphere [Wh/m2].
  Iobh = integrate(integrand, Ts_start, Ts_end)

  return(Iobh$value)
  
  # delta = declination(Ls)
  # 
  # # Hour angles [deg].
  # omega_start = 15 * Ts_start - 180
  # omega_end = 15 * Ts_end - 180
  # 
  # a = (2 * pi * (omega_end - omega_start)) / 360
  # b = sin(phi*pi/180) * sin(delta)
  # c = cos(phi*pi/180) * sin(delta) * (sin(omega_end*pi/180) - sin(omega_start*pi/180))
  # 
  # Iobh = 12/pi * G_ob(Ls) * (a * b + c) 
  # 
  # return(Iobh)
  
}
