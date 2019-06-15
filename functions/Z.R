# Equation 6: Zenith angle of the incident solar radiation [deg]
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# Mars obliquity of rotation axis [deg]
delta_0 = 24.936

# Ls      - Areocentric longitude [deg].
# omega   - Hour angle value [h]. An integer belonging to [6, 18] if using f_89.R or f_90.R.
# phi     - Latitude [deg].
# nfft    - Net flux function type.
function(Ls, omega, phi, nfft){
  
  ########################################
  # Equation 7: Declination angle [rad]. #
  ########################################
  delta = asin(sin(delta_0 * pi/180) * sin(Ls * pi/180))
  
  #################################
  # Equation 8: Hour angle [deg]. #
  #################################
  # From Appelbaum, Joseph & Flood, Dennis. (1990):
  #   The ratio of Mars to Earth length of day is 24.65/24.
  #   It is convenient, for calculation purposes, to define a Mar hour
  #   by dividing the Martian day into 24 hr. Using the same relationship
  #   between the Mars solar time T and the hour angle as for the Earth.
  omega = 15 * omega - 180
  
  ##################################################################
  # Equation 6: Zenith angle of the incident solar radiation [deg] #
  ##################################################################
  a = sin(phi * pi/180) * sin(delta)
  b = cos(phi * pi/180) * cos(delta) * cos(omega * pi/180)
  
  Z = acos(a + b) * 180/pi
  
  # We have to round the zenith angle to a power of ten because the
  # normalized net flux function only takes predetermined Z angle values.
  if(nfft == 1){
    Z = round(Z, -1)
    
    # In case we get 90, replace it with 85. It's actually much closer to 85 than 90 prior to the rounding.
    # Also, we don't have 90 in our normalized net flux function table.
    v1 <- unlist(Z)
    Z = relist(replace(v1, v1==90, 85), skeleton=Z)
    
  }else if(nfft == 2){
    Z = round(Z/5) * 5
    
  }else if(nfft != 3){
    stop(paste("Unsupported net flux function type, should be 1 for f_89, 2 for f_90, or 3 for f: ", nfft))
  }
  
  return(Z)
}