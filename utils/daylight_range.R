# Get a solar time range from sunrise to sunset [h].
#
# TODO: 
#   Range in solar angle radians and degrees?

# Equation 8 (1993): Sunrise.
sunrise = dget(here("utils", "sunrise.R"))

# Equation 9 (1990): Sunset.
sunset = dget(here("utils", "sunset.R"))

# Equation 7 (1990): The declination angle.
source(here("utils", "declination.R"))

daylight_range = function(Ls, phi, T_step=1, T_min=0, T_max=24, beta=NULL, gamma_c=NULL){
  
  # Equation 7 (1990): Declination angle [rad].
  delta = declination(Ls)
  
  # Equations 16 (Update 1991): Figure out if it is polar night or polar day.
  #   Polar night (polar_flag < -1), no solar irradiance.
  #   Polar day (polar_flag > 1), constant solar irradiance.
  polar_flag = -tan(delta) * tan(phi*pi/180)
  
  # Polar night, no solar irradiance.
  if(polar_flag < -1){
    stop("No solar irradiance during polar nights.")
    
  }else if(polar_flag > 1){
    # Polar day, constant solar irradiance.
    return(seq(T_min, T_max, T_step))
    
  }else{
    # For non polar nights or non polar days.
    # Bound time range with sunrise and sunset times.
    T_sr = sunrise(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)
    T_ss = sunset(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)
    
    # Include max solar time if it isn't included already.
    Ts_range = seq(T_sr, T_ss, T_step)
    if(!(T_ss %in% Ts_range)){
      Ts_range = c(Ts_range, T_ss)
    }
    
    # Further bound by desired min and max values.
    Ts_range = Ts_range[Ts_range >= T_min]
    Ts_range = Ts_range[Ts_range <= T_max]
    
    return(Ts_range)
  }
}