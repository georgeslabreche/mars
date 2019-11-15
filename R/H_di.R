#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param tau 
#' @param al 
#' @param beta 
#' @param gamma_c 
#' @param nfft 
#'
#' @return
#' @export
H_di = function(Ls, phi, longitude, tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c, nfft){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
  }
  
  # Hdi is obtained by integrating Idi over the period from sunrise to sunset.
  Hdi = I_di(Ls=Ls, phi=phi, longitude=longitude, tau=tau, T_start=0, T_end=24, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
  
  # Return result.
  return(Hdi)
}