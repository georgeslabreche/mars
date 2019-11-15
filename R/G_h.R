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
#   nfft      - Net flux function implementation type.
#                 - 1 for f_89.
#                 - 2 for f_90.
#                 - 3 for f_analytical.
#
# FIXME: Error check that albedo value is given and not calculated when using look up table for net_flux.
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param T_s 
#' @param z 
#' @param tau 
#' @param al 
#' @param nfft 
#'
#' @return
#' @export
G_h = function(Ls, phi, longitude, T_s=NULL, z=Z(Ls, T_s, phi, nfft), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), nfft){
  
  if(!is_irradiated(Ls=Ls, phi=phi, T_s=T_s, z=z, nfft=nfft)){
    return(0)
    
  }else{
    if(nfft == 1){
      net_flux = f(z=z, tau=tau, al=al, pub_year=1989)

    }else if(nfft == 2){
      net_flux = f(z=z, tau=tau, al=al, pub_year=1990)
      
    }else if(nfft == 3){
      net_flux = f(z=z, tau=tau, al=al)
      
    }else{
      stop("Unsupported net flux function type. Should be 1 for the original 1989 lookup table publication, 2 for the 1990/1991 lookup table update, or 3 for the analytical expression.")
    }
    
    Gh = G_ob(Ls) * cos(z*pi/180) * (net_flux / (1-al))
    
    return(Gh)
  }
}