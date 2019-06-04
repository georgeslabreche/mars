# Equation 6: Zenith angle of the incident solar radiation [deg]
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# Mars obliquity of rotation axis [deg]
delta_0 = 24.936

# Ls          - Areocentric longitude [deg].
# omega       - Hour angle value [h]. An integeder beloging to [6, 18].
# phi         - Latitude [deg].
function(Ls, omega, phi){
  
  ##################################
  # Equation 7: Declination angle. #
  ##################################
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
  
  # We have to round the zenith angle to a power of ten because the
  # normalized net flux function only takes predetermined Z angle values.
  Z = round(acos(a + b) * 180/pi, -1)
  
  # In case we get 90, replace it with 85.
  # Because we don't have 90 in our normalized net flux function table.
  v1 <- unlist(Z)
  Z = relist(replace(v1, v1==90, 85), skeleton=Z)
  #v1 <- unlist(Z)
  #Z = relist(replace(v1, v1==100, 85), skeleton=Z)
}