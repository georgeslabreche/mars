# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface [W/m2-hr].
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

# Equation 6: Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Mars obliquity of rotation axis [W/m2].
delta_0 = 24.936

# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface.
function(Ls, phi, tau, T_start, T_end, nfft){
  
  if(T_start >= T_end){
    stop("Solar start time cannot be after or equal to the solar end time.")
  }
  
  # Convert phi into radians.
  phi = phi * pi/180
  
  # Equation 7 (1990): Declination angle [rad].
  delta = asin(sin(delta_0*pi/180) * sin(Ls*pi/180))
  
  # Equations 16 (Update 1991): Figure out if it is polar night or polar day.
  # For polar nights and polar days:
  #   Polar night (polar_flag < -1), no solar irradiance.
  #   Polar day (polar_flag > 1), constant solar irradiance. 
  polar_flag = -tan(delta) * tan(phi)
   
  # If polar night.
  if(polar_flag < -1){
    # No solar irradiance.
    return(0);
  }
  # If polar day.
  else if(polar_flag > 1){
    # Do nothing, constant solar irradiance.
  }
  # If non polar nights and non polar days.
  else{
    # Constrain T_start and T_end with respect to sunrise and sunset times.

    # Equation 8 (1993): Sunrise hour angle [deg].
    omega_sr = -acos(-tan(phi) * tan(delta)) * 180/pi

    # Equation 9 (1990): Sunset hour angle [deg].
    # Due to symmetry, it's just the sign opposite of the sunrise angle.
    omega_ss = acos(-tan(phi) * tan(delta)) * 180/pi

    # Equation 8 (1990): Hour angle. Determine the Surise and Sunset times [hr].
    # From Appelbaum, Joseph & Flood, Dennis. (1990):
    #   The ratio of Mars to Earth length of day is 24.65/24.
    #   It is convenient, for calculation purposes, to define a Mar hour
    #   by dividing the Martian day into 24 hr. Using the same relationship
    #   between the Mars solar time T and the hour angle as for the Earth.
    T_sr = (omega_sr + 180) / 15
    T_ss = (omega_ss + 180) / 15

    # If start time is after the sunset, then there is no insolation.
    if(T_start > T_ss){
      return(0)
    }
    # If end time is before the sunrise, then there is no insolation.
    else if(T_end < T_sr){
      return(0)
      
    }else{
      # Be careful to cap the start hour angle to that of the sunrise hour angle.
      #   If you do not do this then you will calculate insolation for a 
      #   negative sunset hour angle, i.e. when the sun is below the horizon.
      if(T_start < T_sr){
        # Constrain the given solar time range to times after the sunrise time.
        T_start = T_sr
      }
      
      # Be careful to cap the end hour angle to that of the sunset hour angle.
      #   If you do not do this then you will calculate insolation for a 
      #   negative sunset hour angle, i.e. when the sun is below the horizon.
      if(T_end > T_ss){
        # Constrain the given solar time range to time before to the sunset time.
        T_end = T_ss
      }
    }
  }

  # Can this ever happen?
  if(T_start >= T_end){
    stop("Applying sunrise and sunset constraint has resulted in the start time being after or equal to the solar end time.")
  }
  
  # The interand for Equation 19 (1990).
  interand = function(T_s){
    
    Z = Z_eq(Ls, T_s, phi*180/pi, nfft)
    
    a = Gob_eq(Ls)
    b = cos(Z*pi/180)
    c = exp(-tau / cos(Z*pi/180))
    
    a * b * c
  }
  
  # Equation 19 (1990): Beam hourly insolation on Mars horizontal surface.
  I_bh = integrate(interand, T_start, T_end)
  
  return(I_bh$value)
}


