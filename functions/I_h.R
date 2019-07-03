# Global hourly insolation on Mars horizontal surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# FIXME: Use sunrise, sunset, and is_irradiance util functions

# Equation 4 (1990): Beam irridiance at the top of the Martian atmosphere [W/m2].
Gob_eq = dget(here("functions", "G_ob.R"))

# Equation 6: Zenith angle of the incident solar radiation [deg]
Z_eq = dget(here("functions", "Z.R"))

f = dget(here("functions", "f.R"))

# Check if there is irradiance based on the givent moment.
is_irradiated = dget(here("utils", "is_irradiated.R"))

# Mars obliquity of rotation axis [deg].
delta_0 = 24.936

function(Ls, phi, tau, T_start, T_end, al=0.1, nfft)
{
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
    
    # Equation 8 (1990): Hour angle. Determine the Surise and Sunset times [h].
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
  
  # When applying sunrise and sunset constraint has resulted in the start time being after or equal to the solar end time.
  if(T_start >= T_end){
    return(0)
  }
  
  # The interand for Equation 19 (1990).
  interand = function(T_s){
    Z = Z_eq(Ls=Ls, T_s=T_s, phi=phi*180/pi, nfft=nfft)
    
    if(nfft == 1){
      net_flux = f(Z, tau, al, pub_year=1989)
      
    }else if(nfft == 2){
      net_flux = f(Z, tau, al, pub_year=1990)
      
    }else if(nfft == 3){
      net_flux = f(Z, tau, al)
      
    }else{
      stop("Unsupported net flux function type. Should be 1 for the original 1989 lookup table publication, 2 for the 1990/1991 lookup table update, or 3 for the analytical expression.")
    }
    
    I_obh = Gob_eq(Ls) * cos(Z*pi/180)
    I_h = I_obh * (net_flux/(1-al))
    
    return(I_h)
  }
  
  # Global hourly insolation on Mars horizontal surface.
  I_h = integrate(interand, T_start, T_end)
  
  return(I_h$value)
}





