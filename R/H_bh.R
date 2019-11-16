# Beam daily insolation on Mars horizontal surface  [Wh/m2-day].
#
# Obtained by integrating Equation 19 (1990), beam hourly insolation on Mars horizontal surface,
# over the period from sunrise to sunset.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#

#' Title
#'
#' @param Ls 
#' @param phi 
#' @param tau 
#'
#' @return
#' @export
H_bh = function(Ls, phi, tau){
  
  # Hbh is obtained by integrating Ibh over the period from sunrise to sunset.
  Hbh = I_bh(Ls=Ls, phi=phi, tau=tau, Ts_start=0, Ts_end=24)
  
  # Return result.
  return(Hbh)
}