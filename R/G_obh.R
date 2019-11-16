# Equation 5 (1990): Beam irridiance on a horizontal surface at the top of Mars atmosphere [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param T_s 
#' @param z
#'
#' @return
#' @export
#' TODO: What happens if T_s is null?
G_obh = function(Ls, T_s=NULL, z=Z(Ls=Ls, phi=phi, T_s=T_s)){
  
  Gobh = G_ob(Ls) * cos(z*pi/180)
  return(Gobh)
}