# Equation 6: Zenith angle of the incident solar radiation [deg].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

# Equation 7 (1990): The declination angle.
source(here("utils", "declination.R"))

# Ls      - Areocentric longitude [deg].
# T_s     - Solar time [h]. An integer belonging to [6, 18] if using f_89.R or f_90.R.
# phi     - Latitude [deg].
# nfft    - Net flux function type.
function(Ls, T_s, phi, nfft){
  
  ########################################
  # Equation 7: Declination angle [rad]. #
  ########################################
  delta = declination(Ls)
  
  #################################
  # Equation 8: Hour angle [deg]. #
  #################################
  # From Appelbaum, Joseph & Flood, Dennis. (1990):
  #   The ratio of Mars to Earth length of day is 24.65/24.
  #   It is convenient, for calculation purposes, to define a Mar hour
  #   by dividing the Martian day into 24 hr. Using the same relationship
  #   between the Mars solar time T and the hour angle as for the Earth.
  omega = 15 * T_s - 180
  
  ##################################################################
  # Equation 6: Zenith angle of the incident solar radiation [deg] #
  ##################################################################
  a = sin(phi*pi/180) * sin(delta)
  b = cos(phi*pi/180) * cos(delta) * cos(omega * pi/180)
  
  Z = acos(a + b) * 180/pi
  
  # We have to round the zenith angle to a power of ten because the
  # normalized net flux function only takes predetermined Z angle values.
  if(nfft == 1){
    
    # Round in multiples of 10.
    Z = round(Z, -1)
    
    # There is no column for Z=90 in thenormalized net flux function table.
    # In case Z=90, replace it with 85.
    v = unlist(Z)
    Z = relist(replace(v, v==90, 85), skeleton=Z)
    
  }else if(nfft == 2){
    # Round in multiples of 5.
    Z = round(Z/5) * 5
    
    # There is no column for Z=90 in thenormalized net flux function table.
    # In case Z=90, replace it with 85.
    v = unlist(Z)
    Z = relist(replace(v, v==90, 85), skeleton=Z)
    
  }else if(nfft != 3){
    stop(paste("Unsupported net flux function type, should be 1 for f_89, 2 for f_90, or 3 for f: ", nfft))
  }
  
  return(Z)
}