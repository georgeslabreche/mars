# Equation 16 (1990):  The diffuse irradiance on a horizontal Martian surface.
#
# Gh = Gbh + Gdh
# Determine an expression for the diffuse irradiance based on Eq. 17 and Eq. 18.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

# FIXME: Update this function so that it figures out if its a polar night or day.

# Equation 16 (1990): The solar irradiance components on a horizontal Martian surface [W/m2].
#
#   Ls        - Areocentric longitude [deg].
#   Z         - Sun zenith angle [deg].
#   tau       - Optical depth.
#   al        - Albedo.
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
#' TODO: What happens when Ts is NULL? How is z calculated?
G_dh = function(Ls, phi, longitude, Ts=NULL, z=Z(Ls=Ls, phi=phi, Ts=Ts), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau)){
  
  if(!is_irradiated(Ls=Ls, phi=phi, Ts=Ts, z=z)){
    return(0)
    
  }else{
    Gdh = G_h(Ls=Ls, phi=phi, longitude=longitude, Ts=Ts, z=z, tau=tau, al=al) - G_bh(Ls=Ls, phi=phi, Ts=Ts, z=z, tau=tau)
    return(Gdh)
  }
} 