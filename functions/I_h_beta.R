# Global hourly insolation on Mars inclined surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994).
#   Solar radiation on Mars: Tracking photovoltaic array.
#   Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://www.researchgate.net/publication/24286713_Solar_radiation_on_Mars_Tracking_photovoltaic_array
#
library(here)

# Equation 3 (1994): Global irradiance on Mars inclined surface [W/m2].
Gh_beta_eq = dget(here("functions", "G_h_beta.R"))

# Sunrise.
sunrise = dget(here("utils", "sunrise.R")) 

# Sunset.
sunset = dget(here("utils", "sunset.R")) 

# Polar day.
is_polar_day = dget(here("utils", "is_polar_day.R"))

# Polar night.
is_polar_night = dget(here("utils", "is_polar_night.R"))

function(Ls, phi, tau, T_start, T_end, al=0.1, beta, gamma_c, nfft)
{
  ##################################################################
  # Constrain T_start and T_end based on sunrise and sunset times. #
  # FIXME: Exclude this logic in utils function.                   #
  #        Refactor this function and I_h.R as well.               #
  ##################################################################
  
  if(T_start >= T_end){
    stop("Solar start time cannot be after or equal to the solar end time.")
  }
  
  # If polar night.
  if(is_polar_night(Ls, phi)){
    # No solar irradiance.
    return(0);
  }
  # If polar day.
  else if(is_polar_day(Ls, phi)){
    # No constraining required: constant solar irradiance during polar day.
  }
  # If non polar nights and non polar days.
  else{
    # Constrain T_start and T_end with respect to sunrise and sunset times.
    T_sr = sunrise(Ls, phi, 3)
    T_ss = sunset(Ls, phi, 3)
    
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
  
  #########################
  # Calculate insolation. #
  #########################
  
  # The interand for Equation 19 (1990).
  interand = function(T_s){
    G_h_beta = Gh_beta_eq(Ls=Ls, phi=phi, T_s=T_s, tau=tau, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
    return(G_h_beta)
  }
  
  # Global hourly insolation on Mars horizontal surface.
  I_h_beta = integrate(interand, T_start, T_end)
  
  # Return integration result.
  return(I_h_beta$value)
}
