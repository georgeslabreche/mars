#' Title
#'
#' @param Ls 
#' @param phi 
#' @param longitude 
#' @param Ts 
#' @param z 
#' @param tau 
#' @param al 
#' @param beta
#'
#' @return
#' @export
G_di = function(Ls, phi, longitude, Ts, z=Z(Ls=Ls,  phi=phi, Ts=Ts), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta){
  
  G_di = G_dh(Ls=Ls, phi=phi, longitude=longitude, z=z, tau=tau, al=al) * cos((beta*pi/180) / 2)^2
  return(G_di)
}