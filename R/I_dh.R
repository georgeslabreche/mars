# Diffuse hourly insolation on Mars horizontal surface [Wh/m2].
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
I_dh = function(Ls, phi, longitude, tau, Ts_start, Ts_end, al=albedo(latitude=phi, longitude=longitude, tau=tau)){
  
  Ih = I_h(Ls=Ls, phi=phi, longitude=longitude, tau=tau, Ts_start=Ts_start, Ts_end=Ts_end, al=al) - I_bh(Ls=Ls, phi=phi, tau=tau, Ts_start=Ts_start, Ts_end=Ts_end)
  return(Ih)
}