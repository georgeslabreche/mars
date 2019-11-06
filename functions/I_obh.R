# Equation 11 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#

library(here)

# Equation 4 (1990): Beam irridiance at the top of the Martian atmosphere [W/m2].
Gob_eq = dget(here("functions", "G_ob.R"))

# Equation 6: Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Constrain T_start and T_end based on sunrise and sunset times.
constrain_solar_time_range = dget(here("utils", "constrain_solar_time_range.R")) 

function(Ls, phi, T_start, T_end, nfft){
  
  # Step 1: Constrain T_start and T_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls, phi, T_start, T_end)
  
  # No solar irradiance within the contrained time range.
  if(is.null(T_range)){
    return(0)
    
  }else{
    # Constrain the time range.
    T_start = T_range$T_start
    T_end = T_range$T_end
  }
  
  # Step 2: Calculate insolation.
  
  # # Equation 8 (1990): Hour angle [deg].
  # # From Appelbaum, Joseph & Flood, Dennis. (1990):
  # #   The ratio of Mars to Earth length of day is 24.65/24.
  # #   It is convenient, for calculation purposes, to define a Mar hour
  # #   by dividing the Martian day into 24 hr. Using the same relationship
  # #   between the Mars solar time T and the hour angle as for the Earth.
  # omega_start = 15 * T_start - 180
  # omega_end = 15 * T_end - 180
  # 
  # # Equation 7 (1990): Declination angle [rad].
  # delta = declination(Ls)
  #   
  # # Equation 12 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
  # w = (12/pi) * Gob_eq(Ls) # FIXME: Should this be 12.33/pi ?
  # x = (2 * pi * (omega_end - omega_start)) / 360
  # y = sin(phi * pi/180) * sin(delta)
  # z = cos(phi * pi/180) * cos(delta) * (sin(omega_end * pi/180) - sin(omega_start * pi/180))
  # 
  # Iobh = w * (x * y + z)
  
  # The interand for Equation 11 (1990).
  interand = function(T_s){
    Z = Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft)
    
    a = Gob_eq(Ls)
    b = cos(Z*pi/180)
    
    a * b
  }
  
  # Equation 11 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
  I_obh = integrate(interand, T_start, T_end)
  
  return(I_obh$value)
}