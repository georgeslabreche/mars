# Diffuse daily insolation on Mars horizontal surface [Wh/m2-day].
#
# Obtained by integrating I_dh, diffuse hourly insolation on Mars horizontal surface,
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
#' @param al 
#' @param nfft 
#'
#' @return
#' @export
H_dh = function(Ls, phi, tau, al, nfft){
  
  # Integrate I_dh over the period from sunrise to sunset.
  Hdh = I_dh(Ls, phi, tau, 0, 24, al, nfft)
  
  # Return result.
  return(Hdh)
}