
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param tau 
#' @param beta 
#' @param gamma_c
#'
#' @return
#' @export
H_bi = function(Ls, phi, tau, beta, gamma_c){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
  }
  
  # Hbi is obtained by integrating Ibi over the period from sunrise to sunset.
  Hbi = I_bi(Ls=Ls, phi=phi, tau=tau, Ts_start=0, Ts_end=24, beta=beta, gamma_c=gamma_c)
  
  # Return result.
  return(Hbi)
}