# Equation 18: Beam irradiance on Mars horizontal surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

# FIXME: Update this function so that it figures out if its a polar night or day.

# Equation 18: Beam irradiance on Mars horizontal surface [W/m2].
#
#   Ls        - Areocentric longitude [deg].
#   phi       -
#   longitude - NOT NEEDED - Included for looping convenience with other functions.
#   Z         - Sun zenith angle [deg].
#   tau       - Optical depth.
#   al        - NOT NEEDED - Included for looping convenience with other functions.
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param Ts 
#' @param z 
#' @param tau 
#' @param al
#'
#' @return
#' @export
G_bh = function(Ls, phi, Ts, z=Z(Ls=Ls, phi=phi, Ts=Ts), tau){
  
  if(!is_irradiated(Ls=Ls, phi=phi, Ts=Ts, z=z)){
    return(0)
    
  }else{
    G_ob(Ls) * cos(z * pi/180) * exp(-tau / cos(z*pi/180))
  }
} 