Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = TRUE)

# TODO: Write a test script.
#' Check if there is solar irradiance at the given location and moment.
#'
#' @param Ls 
#' @param phi 
#' @param Ts 
#' @param z 
#' @param beta 
#' @param gamma_c
#'
#' @return
#' @export
is_irradiated = function(Ls, phi, Ts, z=Z(Ls, Ts, phi), beta=NULL, gamma_c=NULL){

  if(isTRUE(identical(z, numeric(0)))){
    stop("One of the following is required: i. Sun zenith angle z [deg] or ii. Both latitude phi [deg] and solar time Ts [h].")
  
  # FIXME: Is this needed?  
  }else if(!is.null(phi) && !is.null(Ts) && z != Z(Ls=Ls, Ts=Ts, phi=phi)){
    if(isTRUE(show_net_flux_function_warnings())){
      message("Sun zenith angle z [deg] has been provided, ignoring given latitude phi [deg] and solar time Ts [h].")
    }
  }else if(is.null(phi) && !is.null(Ts) || !is.null(phi) && is.null(Ts)) {
    if(isTRUE(show_net_flux_function_warnings())){
      message("A latitude phi [deg] or a solar time Ts [h] has been given but not needed because a Sun zenith angle Z [deg] has been given as well.")
    }
  }else if(!is.null(phi) && !is.null(Ts)){
 
    # There is no irradiance during polar nights.
    if(is_polar_night(Ls=Ls, phi=phi)){
      return(FALSE)
      
    }else if(is_polar_day(Ls=Ls, phi=phi)){
      # Constant solar irradiance during polar day.
      # The sun is out all the time.
      return(TRUE)
      
    }else{
      # There is no irradiance if the solar time is before sunrise or after sunset.
      Tsr = sunrise(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)
      Tss = sunset(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)

      if(Ts < Tsr || Ts > Tss){
        return(FALSE)
        
      }else{
        return(TRUE)
      }
    }
  }
  
  # FIXME: Use ifelse function to support scalar?
  # Only do this check if a z scalar is given rather than a vector (e.g. from integrating to calculate daily insolation)
  if(length(z) == 1){
    if(z >= 90){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }else{
    return(TRUE)
  }
}