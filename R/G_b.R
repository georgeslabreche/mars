# Equation 14 (1990): Direct beam irradiance on the Mars surface normal to the solar rays.
#
# From Appelbaum, Joseph & Flood, Dennis. (1990):
#   The direct beam irradiance, Gb, on the Martian surface normal to the solar rays
#   is related by Beer's law to the optical depth, tau, of the intervening atmospheric
#   haze [...] where m(z) is the air mass determined by the zenith angle Z.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

# Equation 14 (1990): Beam irradiance on Mars surface [W/m2]
#   Ls    - Areocentric Longitude.
#   Z     - Sun Zenith Angle.
#   tau   - Optical Depth.
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param Ts 
#' @param z 
#' @param tau
#'
#' @return
#' @export
G_b = function(Ls, phi=NULL, Ts=NULL, z=Z(Ls=Ls, phi=phi, Ts=Ts), tau){
  
  if(!is_irradiated(Ls=Ls, phi=phi, Ts=Ts, z=z)){
    return(0)
    
  }else{
    G_ob(Ls) * exp(-tau / cos(z*pi/180))
  }
}