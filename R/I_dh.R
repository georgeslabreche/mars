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
#' @param T_start 
#' @param T_end 
#' @param al 
#' @param nfft 
#'
#' @return
#' @export
I_dh = function(Ls, phi, longitude, tau, T_start, T_end, al=albedo(latitude=phi, longitude=longitude, tau=tau), nfft){
  
  Ih = I_h(Ls=Ls, phi=phi, longitude=longitude, tau=tau, T_start=T_start, T_end=T_end, al=al, nfft=nfft) - I_bh(Ls=Ls, phi=phi, tau=tau, T_start=T_start, T_end=T_end, al=al, nfft=nfft)
  return(Ih)
}