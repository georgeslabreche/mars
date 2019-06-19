# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface.
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

# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface.
test = function(Ls, phi, tau, T_start, T_end){
  
  # Convert phi into radians.
  phi = phi * pi/180
  
  # Equation 7 (1990): Declination angle [rad].
  delta = asin(sin(delta_0*pi/180) * sin(Ls*pi/180))
  
  interand = function(T_s){
    # Equation 8 (1990): Hour angle [rad].
    # From Appelbaum, Joseph & Flood, Dennis. (1990):
    #   The ratio of Mars to Earth length of day is 24.65/24.
    #   It is convenient, for calculation purposes, to define a Mar hour
    #   by dividing the Martian day into 24 hr. Using the same relationship
    #   between the Mars solar time T and the hour angle as for the Earth.
    omega = (15 * T_s - 180) * pi/180
    
    # Equation 19 (1990): Beam hourly insolation on Mars horizontal surface.
    a = 12/pi * Gob_eq(Ls)
    b = sin(phi) * sin(delta) + cos(phi) * cos(delta) * cos(omega)
    c = exp(-tau / (sin(phi) * sin(delta) + cos(phi) * cos(delta) * cos(omega)))
    
    a * b * c
  }
  
  I_bh = integrate(interand, T_start, T_end)
  
  print(I_bh)
  
  stop("Not yet implemented.")
}

# Result should be 390
test(Ls=69, phi=22.3, tau=0.65, T_start=13, T_end=14)

