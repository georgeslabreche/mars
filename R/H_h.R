# Global daily insolation on Mars horizontal surface [Wh/m2-day].
#
# Obtained by integrating I_h, global hourly insolation on Mars horizontal surface, 
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
#' @param longitude
#' @param tau 
#' @param al
#'
#' @return
#' @export
H_h = function(Ls, phi, longitude, tau, al=albedo(latitude=phi, longitude=longitude, tau=tau)){

  # Hh is obtained by integrating Ih over the period from sunrise to sunset.
  Hh = I_h(Ls=Ls, phi=phi, longitude=longitude, tau=tau, Ts_start=0, Ts_end=24, al=al)
  
  # Return result.
  return(Hh)
}
              
              
              