#
# Constrain Ts_start and Ts_end based on sunrise and sunset times.
#FIXME: Refactor to daylight_range without Ts_start and Ts_end.


#' Title
#'
#' @param Ls 
#' @param phi 
#' @param Ts_start 
#' @param Ts_end 
#' @param beta 
#' @param gamma_c 
#'
#' @return
#' @export
constrain_solar_time_range = function(Ls, phi, Ts_start, Ts_end, beta=NULL, gamma_c=NULL){
  
  if(Ts_start >= Ts_end){
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
      list(Ts_start=Ts_start,
           Ts_end=Ts_end)
    )
  }
  # If non polar nights and non polar days.
  else{
    # Constrain Ts_start and Ts_end with respect to sunrise and sunset times.
    Tsr = sunrise(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)
    Tss = sunset(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)
    
    # If start time is after the sunset, then there is no solar irradiance.
    if(Ts_start > Tss){
      return(NULL)
    }
    # If end time is before the sunrise, then there is no solar irradiance.
    else if(Ts_end < Tsr){
      return(NULL)
      
    }else{
      # Be careful to cap the start hour angle to that of the sunrise hour angle.
      #   If you do not do this then you will calculate insolation for a 
      #   negative sunset hour angle, i.e. when the sun is below the horizon.
      if(Ts_start < Tsr){
        # Constrain the given solar time range to times after the sunrise time.
        Ts_start = Tsr
      }
      
      # Be careful to cap the end hour angle to that of the sunset hour angle.
      #   If you do not do this then you will calculate insolation for a 
      #   negative sunset hour angle, i.e. when the sun is below the horizon.
      if(Ts_end > Tss){
        # Constrain the given solar time range to time before to the sunset time.
        Ts_end = Tss
      }
    }
  }
  
  # When applying sunrise and sunset constraint has resulted in the start time being after or equal to the solar end time.
  if(Ts_start >= Ts_end){
    return(NULL)
  }
  
  # Return the contrained time range.
  return(
    list(Ts_start=Ts_start,
         Ts_end=Ts_end)
  )
}