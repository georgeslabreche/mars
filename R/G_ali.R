
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
G_ali = function(Ls, phi, longitude, Ts, z=Z(Ls=Ls, Ts=Ts, phi=phi), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta){
  
  Gali = al * G_h(Ls=Ls, phi=phi, longitude=longitude, z=z, tau=tau, al=al) * sin((beta*pi/180) / 2)^2
  
  # If negative then interpret as no albedo irradiance hitting the inclined surface.
  # TODO: Check if negative value is actually possible.
  Gali = ifelse(Gali < 0, 0, Gali)
  return(Gali)
}