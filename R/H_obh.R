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
#' FIXME: Use I_obh.
H_obh = function(Ls, phi){
  # FIXME: Support both?
  # Iobh =I_obh(Ls=Ls, phi=phi, T_start=0, T_end=24)
  # return(Iobh)

  # Equation 7 (1990): Declination angle [rad].
  delta = declination(Ls)

  # Equation 9 (1990): The sunset hour angle [rad].
  omega_ss = sunset(Ls=Ls, phi=phi, unit=1)

  # Equation 13 (1990): Daily beam insolation on a horizontal surfce at top of Mars atmosphere [Wh/m2-day].
  a = (24/pi) * G_ob(Ls)
  b = 2 * pi * (omega_ss*180/pi) / 360
  c = sin(phi*pi/180) * sin(delta)
  d = cos(phi*pi/180) * cos(delta) * sin(omega_ss)

  Hobh = a * (b * c + d)

  # Return result.
  return(Hobh)
}
