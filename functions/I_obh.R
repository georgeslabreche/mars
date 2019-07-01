# Equation 12 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#

# FIXME - Account for:
#   - Polar nights. See Equation 14 (Update 1990) and refinements in Equations 16 (Update 1991).
#   - Polar days. See Equation 14 (Update 1990) and refinements in Equations 16 (Update 1991).

library(here)

# Equation 4 (1990): Beam irridiance at the top of the Martian atmosphere [W/m2].
Gob_eq = dget(here("functions", "G_ob.R"))

# Mars obliquity of rotation axis [deg].
delta_0 = 24.936

function(Ls, phi, T_start, T_end){
  
  if(T_start >= T_end){
    stop("Solar start time cannot be after or equal to the solar end time.")
  }
  
  # Convert phi into radians.
  phi = phi * pi/180
  
  # Equation 7 (1990): Declination angle [rad].
  delta = asin(sin(delta_0*pi/180) * sin(Ls*pi/180))
  
  # Equation 8 (1993): Sunrise hour angle.
  omega_sr_rad = -acos(-tan(phi) * tan(delta))
  omega_sr_deg = omega_sr_rad * 180/pi
  
  # Equation 9 (1990): Sunset hour angle.
  # Due to symmetry, it's just the sign opposite of the sunrise angle.
  omega_ss_rad = acos(-tan(phi) * tan(delta))
  omega_ss_deg = omega_ss_rad * 180/pi
  
  # Equation 8 (1990): Hour angle.
  # From Appelbaum, Joseph & Flood, Dennis. (1990):
  #   The ratio of Mars to Earth length of day is 24.65/24.
  #   It is convenient, for calculation purposes, to define a Mar hour
  #   by dividing the Martian day into 24 hr. Using the same relationship
  #   between the Mars solar time T and the hour angle as for the Earth.
  omega_start_deg = 15 * T_start - 180
  omega_end_deg = 15 * T_end - 180
  
  # If your start time is after the sunset, then there is no insolation.
  if(omega_start_deg > omega_ss_deg){
    Iobh = 0
  }
  
  # If your end time is before the sunrise, then there is no insolation.
  else if(omega_end_deg < omega_sr_deg){
    Iobh = 0
    
  }else{
    # Be careful to cap the start hour angle to that of the sunrise hour angle.
    #   If you do not do this then you will calculate insolation for a 
    #   negative sunset hour angle, i.e. when the sun is below the horizon.
    if(omega_start_deg < omega_sr_deg){
      omega_start_deg = omega_sr_deg
    }
    omega_start_rad = omega_start_deg * pi/180
    
    
    # Be careful to cap the end hour angle to that of the sunset hour angle.
    #   If you do not do this then you will calculate insolation for a 
    #   negative sunset hour angle, i.e. when the sun is below the horizon.
    if(omega_end_deg > omega_ss_deg){
      omega_end_deg = omega_ss_deg
    }
    omega_end_rad = omega_end_deg * pi/180
    
    # Can this ever happen?
    if(omega_start_deg >= omega_end_deg){
      stop("Applying sunrise and sunset constraint has resulted in the start time being after or equal to the solar end time.")
    }
    
    # Equation 12 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
    w = (12/pi) * Gob_eq(Ls)
    x = (2 * pi * (omega_end_deg - omega_start_deg)) / 360
    y = sin(phi) * sin(delta)
    z = cos(phi) * cos(delta) * (sin(omega_end_rad) - sin(omega_start_rad))
    
    Iobh = w * (x * y + z)
  }
  
  return(Iobh)
}
