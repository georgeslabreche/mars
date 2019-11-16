# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
#   Ls        - Areocentric longitude [deg].
#   phi       - Planetary latitude [deg].
#   Ts        - Solar Time [h].
#   Z         - Sun zenith angle [deg].
#   tau       - Optical depth.
#   al        - Albedo.
#
# FIXME: Error check that albedo value is given and not calculated when using look up table for net_flux.



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
G_h = function(Ls, phi, longitude, Ts=NULL, z=Z(Ls=Ls, phi=phi, Ts=Ts), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau)){
  
  if(!is_irradiated(Ls=Ls, phi=phi, Ts=Ts, z=z)){
    return(0)
    
  }else{
    # Get the normalized net flux value.
    net_flux = f(z=z, tau=tau, al=al)
    
    # Calculate Gh.
    Gh = G_ob(Ls) * cos(z*pi/180) * (net_flux / (1-al))
    
    # Return the result.
    return(Gh)
  }
}