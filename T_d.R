# Equation 10 (1990): Number of Mars daylight hours.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# Mars obliquity of rotation axis [W/m2].
delta_0 = 24.936

# The function.
#   Ls      - Areocentric longitude [deg].
#   phi     - Latitude [deg].
function(Ls, phi){
  # Equation 7 (1990): Declination angle [rad].
  delta = asin(sin(delta_0*pi/180) * sin(Ls*pi/180))
  
  # Equation 10 (1990).
  Td = (2/15) * acos(-tan(phi*pi/180) * tan(delta))

  return(Td)
}