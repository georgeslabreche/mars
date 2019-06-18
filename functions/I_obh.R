# Equation 12 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
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

test = function(Ls, phi, T_start, T_end){
  
  # Equation 7 (1990): Declination angle [rad].
  delta = asin(sin(delta_0*pi/180) * sin(Ls*pi/180))
  
  # Equation 8 (1990): Hour angle.
  # From Appelbaum, Joseph & Flood, Dennis. (1990):
  #   The ratio of Mars to Earth length of day is 24.65/24.
  #   It is convenient, for calculation purposes, to define a Mar hour
  #   by dividing the Martian day into 24 hr. Using the same relationship
  #   between the Mars solar time T and the hour angle as for the Earth.
  omega_start_deg = 15 * T_start - 180 
  omega_start_rad = omega_start_deg * pi/180
  
  omega_end_deg = 15 * T_end - 180
  omega_end_rad = omega_end_deg * pi/180
  
  # Convert phi into radians.
  phi = phi * pi/180
  
  # Equation 12 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
  a = (12/pi) * Gob_eq(Ls)
  b = (2 * pi * (omega_end_deg - omega_start_deg)) / 360
  c = sin(phi) * sin(delta)
  d = cos(phi) * cos(delta) * (sin(omega_end_rad) - sin(omega_start_rad))
  
  Iobh = a * (b * c + d)
  #print(paste("Analytical: ", Iobh))
  
  integrand = function(omega) {
    x = sin(phi) * sin(delta)
    y = cos(phi) * cos(delta) * cos(omega)
    z = (12/pi) * Gob_eq(Ls)
    Ibh_omega = z * (x + y)
    
    return(Ibh_omega)
  }
  
  Iobh2 = integrate(integrand, lower=omega_start_rad, upper=omega_end_rad)
  #print(paste("Integral: ", Iobh2))
  
  return(Iobh2) #FIXME: Negative numbers when not expected. Try with Eq 11.
}

I_obh = test(69, 22.3, 18, 19) 
print(I_obh)