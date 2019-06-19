# Equation 13 (1990): Daily beam insolation on a horizontal surfce at top of Mars atmosphere [Wh/m2-day].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#

library(here)

# Equation 4 (1990): Beam irridiance at the top of the Martian atmosphere [W/m2].
Gob_eq = dget(here("functions", "G_ob.R"))

# Mars obliquity of rotation axis [W/m2].
delta_0 = 24.936

function(Ls, phi){
  # Convert phi into radians.
  phi = phi * pi/180
  
  # Equation 7 (1990): Declination angle [rad].
  delta = asin(sin(delta_0*pi/180) * sin(Ls*pi/180))
  
  # Equation 9 (1990): The sunset hour angle [rad].
  omega_ss = acos(-tan(phi) * tan(delta))
  
  # Equation 13 (1990): Daily beam insolation on a horizontal surfce at top of Mars atmosphere [Wh/m2-day].
  a = (24/pi) * Gob_eq(Ls)
  b = 2 * pi * (omega_ss*180/pi) / 360 # Check is this omega is meant to be rad or degrees. Probably degrees.
  c = sin(phi) * sin(delta)
  d = cos(phi) * cos(delta) * sin(omega_ss)
  
  H_obh = a * (b * c + d)
  
  return(H_obh)
}
