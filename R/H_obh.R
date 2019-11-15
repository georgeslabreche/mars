# Equation 13 (1990): Daily beam insolation on a horizontal surfce at top of Mars atmosphere [Wh/m2-day].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
#' Title
#'
#' @param Ls 
#' @param phi 
#'
#' @return
#' @export
H_obh = function(Ls, phi){
  
  # Convert phi into radians.
  phi = phi * pi/180
  
  # Equation 7 (1990): Declination angle [rad].
  delta = declination(Ls)
  
  # Equation 9 (1990): The sunset hour angle [rad].
  omega_ss = acos(-tan(phi) * tan(delta))
  
  # Equation 13 (1990): Daily beam insolation on a horizontal surfce at top of Mars atmosphere [Wh/m2-day].
  a = (24/pi) * Gob_eq(Ls)
  b = 2 * pi * (omega_ss*180/pi) / 360
  c = sin(phi) * sin(delta)
  d = cos(phi) * cos(delta) * sin(omega_ss)
  
  Hobh = a * (b * c + d)
  
  # Return result.
  return(Hobh)
}
