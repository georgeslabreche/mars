# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#


# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface.
# 
#   al  - NOT NEEDED - Included for looping convenience with other functions.
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param tau 
#' @param T_start 
#' @param T_end 
#'
#' @return
#' @export
I_bh = function(Ls, phi, tau, T_start, T_end){

  # Step 1: Constrain T_start and T_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls=Ls, phi=phi, T_start=T_start, T_end=T_end)

  # No solar irradiance within the contrained time range.
  if(is.null(T_range)){
    return(0)

  }else{
    # Constrain the time range.
    T_start = T_range$T_start
    T_end = T_range$T_end
  }
  
  # Step 2: Calculate beam insolation.

  # The integrand for Equation 19 (1990).
  integrand = function(T_s){

    z = Z(Ls=Ls, phi=phi, T_s=T_s)

    a = G_ob(Ls)
    b = cos(z*pi/180)
    c = exp(-tau / cos(z*pi/180))

    a * b * c
  }

  # Equation 19 (1990): Beam hourly insolation on Mars horizontal surface.
  Ibh = integrate(integrand, T_start, T_end)

  return(Ibh$value)
}

