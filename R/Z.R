#' Equation 6: Zenith angle of the incident solar radiation [deg].
#'
#' Source: Equation 6 in Appelbaum, Joseph & Flood, Dennis. (1990).
#' 
#' @param Ls Areocentric longitude [deg].
#' @param phi Planetary latitude [deg].
#' @param T_s Solar time [h]
#'
#' @return
#' @export
Z = function(Ls, phi, T_s){
  
  # Equation 7: Declination angle [rad].
  delta = declination(Ls)
  
  # Equation 8: Hour angle [deg].
  omega = 15 * T_s - 180
  
  # Equation 6: Zenith angle of the incident solar radiation [deg].
  a = sin(phi*pi/180) * sin(delta)
  b = cos(phi*pi/180) * cos(delta) * cos(omega * pi/180)
  
  # Calculate z.
  z = acos(a + b) * 180/pi
  
  # Return result.
  return(z)
}