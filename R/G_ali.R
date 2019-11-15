#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param T_s 
#' @param Z 
#' @param tau 
#' @param al 
#' @param beta 
#' @param nfft 
#'
#' @return
G_ali = function(Ls, phi, longitude, T_s, Z=Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, nfft){
  
  Gali = al * Gh_eq(Ls=Ls, phi=phi, longitude=longitude, Z=Z, tau=tau, al=al, nfft=nfft) * sin((beta*pi/180) / 2)^2
  return(Gali)
}

