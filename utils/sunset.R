library(here)
sunrise = dget(here("utils", "sunrise.R"))

# The function.
#   Ls    - Areocentric longitude.
#   phi   - Planetary latitude.
#   unit  - Unit to return:
#           - 1 for radians
#           - 2 for degrees
#           - 3 for solar hour.
function(Ls, phi, unit=1){
  
  # Equation 8 (1993): Sunrise hour angle [rad].
  omega_rad = -sunrise(Ls, phi)
  
  # If polar night or polar day, then there is no sunrise or sunset.
  if(is.na(omega_rad)){
    return(NA)
  }
  
  omega_deg = omega_rad * 180/pi
  
  # Equation 8 (1990): Hour angle. Determine the Surise and Sunset times [h].
  # From Appelbaum, Joseph & Flood, Dennis. (1990):
  #   The ratio of Mars to Earth length of day is 24.65/24.
  #   It is convenient, for calculation purposes, to define a Mar hour
  #   by dividing the Martian day into 24 hr. Using the same relationship
  #   between the Mars solar time T and the hour angle as for the Earth.
  Ts = (omega_deg + 180) / 15
  
  if(unit == 1){
    return(omega_rad)
    
  }else if(unit == 2){
    return(omega_deg)
    
  }else if(unit == 3){
    return(Ts)
    
  }else{
    stop("Sunrise option must either be 1 for radians, 2 for degrees, or 3 for solar hour.")
  }
}