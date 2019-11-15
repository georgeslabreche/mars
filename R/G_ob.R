# Constants.
Ls_P = 248      # Periphelion.
e = 0.093377    # Mars orbit eccentricity.
Mb = 590        # Mean beam irradiance at the top of the Martian atmosphere.

#' Equation 4 (1990): Beam irridiance at the top of the Martian atmosphere [W/m2].
#' 
#' Source: 
#'   Appelbaum, Joseph & Flood, Dennis. (1990).
#'   Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#'   https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars
#'   
#' @param Ls Areocentric longitude.
#'
#' @return Beam irradiance [W/m2].
#' @export
G_ob = function(Ls){
  
  Gob = Mb * (1 + e*cos((Ls-Ls_P)*pi/180))^2 / (1-e^2)^2
  return(Gob)
}
