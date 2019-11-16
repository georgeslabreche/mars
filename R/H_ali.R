
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param tau 
#' @param al 
#' @param beta 
#' @param gamma_c
#'
#' @return
#' @export
H_ali = function(Ls, phi, longitude, tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
  }
  
  # H_alii is obtained by integrating I_ali over the period from sunrise to sunset.
  Hali = I_ali(Ls=Ls, phi=phi, longitude=longitude, tau=tau, Ts_start=0, Ts_end=24, al=al, beta=beta, gamma_c=gamma_c)
  
  # Return result.
  return(Hali)
}