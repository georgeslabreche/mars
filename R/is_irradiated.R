# TODO: Write a test script.
#' Check if there is solar irradiance at the given location and moment.
#'
#' @param Ls 
#' @param phi 
#' @param T_s 
#' @param z 
#' @param beta 
#' @param gamma_c 
#' @param nfft 
#'
#' @return
#' @export
is_irradiated = function(Ls, phi, T_s, z=Z(Ls, T_s, phi, nfft), beta=NULL, gamma_c=NULL, nfft){


  if(isTRUE(identical(z, numeric(0)))){
    stop("One of the following is required: i. Sun zenith angle z [deg] or ii. Both latitude phi [deg] and solar time T_s [h].")
  
  # FIXME: Is this needed?  
  }else if(!is.null(phi) && !is.null(T_s) && z != Z(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft)){
    message("Sun zenith angle z [deg] has been provided, ignoring given latitude phi [deg] and solar time T_s [h].")
    
  }else if(is.null(phi) && !is.null(T_s) || !is.null(phi) && is.null(T_s)) {
    message("A latitude phi [deg] or a solar time T_s [h] has been given but not needed because a Sun zenith angle Z [deg] has been given as well.")
    
  }else if(!is.null(phi) && !is.null(T_s)){
 
    # There is no irradiance during polar nights.
    if(is_polar_night(Ls=Ls, phi=phi)){
      return(FALSE)
      
    }else if(is_polar_day(Ls=Ls, phi=phi)){
      # Constant solar irradiance during polar day.
      # The sun is out all the time.
      return(TRUE)
      
    }else{
      # There is no irradiance if the solar time is before sunrise or after sunset.
      T_sr = sunrise(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)
      T_ss = sunset(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)

      if(T_s < T_sr || T_s > T_ss){
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