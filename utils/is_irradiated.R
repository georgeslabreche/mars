# Check if there is solar irradiance at the given location and moment.

# Equation 6: Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Equation 8 (1993): Sunrise.
sunrise = dget(here("utils", "sunrise.R"))

# Equation 9 (1990): Sunset.
sunset = dget(here("utils", "sunset.R"))

# Polar day.
is_polar_day = dget(here("utils", "is_polar_day.R"))

# Polar night.
is_polar_night = dget(here("utils", "is_polar_night.R"))


function(Ls, phi, T_s, Z=Z_eq(Ls, T_s, phi, nfft), nfft){

  if(isTRUE(identical(Z, numeric(0)))){
    stop("One of the following is required: i. Sun zenith angle Z [deg] or ii. Both latitude phi [deg] and solar time T_s [h].")
    
  }else if(!is.null(phi) && !is.null(T_s) && Z != Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft)){
    warning("Sun zenith angle Z [deg] has been provided, ignoring given latitude phi [deg] and solar time T_s [h].")
    
  }else if(is.null(phi) && !is.null(T_s) || !is.null(phi) && is.null(T_s)) {
    warning("A latitude phi [deg] or a solar time T_s [h] has been given but not needed because a Sun zenith angle Z [deg] has been given as well.")
    
  }else if(!is.null(phi) && !is.null(T_s)){
 
    # There is no irradiance during polar nights.
    if(is_polar_night(Ls, phi)){
      return(FALSE)
      
    }else if(is_polar_day(Ls, phi)){
      # Constant solar irradiance during polar day.
      # The sun is out all the time.
      return(TRUE)
      
    }else{
      # There is no irradiance if the solar time is before sunrise or after sunset.
      T_sr = sunrise(Ls, phi, 3)
      T_ss = sunset(Ls, phi, 3)

      if(T_s < T_sr || T_s > T_ss){
        return(FALSE)
        
      }else{
        return(TRUE)
      }
    }
  }
  
  # Only do this check if a Z scalar is given rather than a vector (e.g. from integrating to calculate daily insolation)
  if(length(Z) == 1){
    if(Z >= 90){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }else{
    return(TRUE)
  }
}