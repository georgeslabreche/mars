#
# Constrain T_start and T_end based on sunrise and sunset times.
#FIXME: Refactor to daylight_range without T_start and T_end.


#' Title
#'
#' @param Ls 
#' @param phi 
#' @param T_start 
#' @param T_end 
#' @param beta 
#' @param gamma_c 
#'
#' @return
#' @export
constrain_solar_time_range = function(Ls, phi, T_start, T_end, beta=NULL, gamma_c=NULL){
  
  if(T_start >= T_end){
    stop("Solar start time cannot be after or equal to the solar end time.")
  }
  
  # If polar night.
  if(is_polar_night(Ls=Ls, phi=phi)){
    # No solar irradiance.
    return(NULL);
  }
  # If polar day.
  else if(is_polar_day(Ls=Ls, phi=phi)){
    # No constraining required: constant solar irradiance during polar day.
    # FIXME: What about for inclined surface?
    #        As in, what if the Sun is just above the horizon on the back of the inclined surface?
    return(
      list(T_start=T_start,
           T_end=T_end)
    )
  }
  # If non polar nights and non polar days.
  else{
    # Constrain T_start and T_end with respect to sunrise and sunset times.
    T_sr = sunrise(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)
    T_ss = sunset(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)
    
    # If start time is after the sunset, then there is no solar irradiance.
    if(T_start > T_ss){
      return(NULL)
    }
    # If end time is before the sunrise, then there is no solar irradiance.
    else if(T_end < T_sr){
      return(NULL)
      
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
    return(NULL)
  }
  
  # Return the contrained time range.
  return(
    list(T_start=T_start,
         T_end=T_end)
  )
}