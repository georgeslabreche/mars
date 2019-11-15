#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param T_s 
#' @param z 
#' @param tau 
#' @param al 
#' @param beta 
#' @param nfft 
#'
#' @return
#' @export
G_di = function(Ls, phi, longitude, T_s, z=Z(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, nfft){
  
  G_di = Gdh_eq(Ls=Ls, phi=phi, longitude=longitude, z=z, tau=tau, al=al, nfft=nfft) * cos((beta*pi/180) / 2)^2
  return(G_di)
}