
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param tau 
#' @param beta 
#' @param gamma_c 
#' @param nfft 
#'
#' @return
#' @export
H_bi = function(Ls, phi, tau, beta, gamma_c, nfft){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
  }
  
  # Hbi is obtained by integrating Ibi over the period from sunrise to sunset.
  Hbi = I_bi(Ls=Ls, phi=phi, tau=tau, T_start=0, T_end=24, beta=beta, gamma_c=gamma_c, nfft=nfft)
  
  # Return result.
  return(Hbi)
}