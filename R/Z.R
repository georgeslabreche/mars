#' Equation 6: Zenith angle of the incident solar radiation [deg].
#'
#' Source: Equation 6 in Appelbaum, Joseph & Flood, Dennis. (1990).
#' 
#' @param Ls Areocentric longitude [deg].
#' @param T_s Solar time [h]
#' @param phi Planetary latitude [deg].
#' @param nfft Net flux function type.
#'
#' @return
#' @export
Z = function(Ls, T_s, phi, nfft=3){
  
  # Equation 7: Declination angle [rad].
  delta = declination(Ls)
  
  # Equation 8: Hour angle [deg].
  omega = 15 * T_s - 180
  
  # Equation 6: Zenith angle of the incident solar radiation [deg].
  a = sin(phi*pi/180) * sin(delta)
  b = cos(phi*pi/180) * cos(delta) * cos(omega * pi/180)
  
  z = acos(a + b) * 180/pi
  
  # We have to round the zenith angle to a power of ten because the
  # normalized net flux function only takes predetermined Z angle values.
  if(nfft == 1){
    
    # Round in multiples of 10.
    z = round(z, -1)
    
    # There is no column for Z = 90° in thenormalized net flux function table.
    # In case Z = 90°, replace it with 85°.
    v = unlist(z)
    z = relist(replace(v, v==90, 85), skeleton=z)
    
  }else if(nfft == 2){
    # Round in multiples of 5.
    z = round(z/5) * 5
    
    # There is no column for z = 90° in thenormalized net flux function table.
    # In case z = 90°, replace it with 85.
    v = unlist(z)
    z = relist(replace(v, v==90, 85), skeleton=z)
    
  }else if(nfft != 3){
    stop(paste("Unsupported net flux function type, should be 1 for f_89, 2 for f_90, or 3 for f: ", nfft))
  }
  
  return(z)
}