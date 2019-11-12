library(here)

Gdi_eq = dget(here("functions", "G_di.R"))

# Constrain T_start and T_end based on sunrise and sunset times.
constrain_solar_time_range = dget(here("utils", "constrain_solar_time_range.R")) 

source(here("functions", "albedo.R"))

function(Ls, phi, longitude, tau, T_start, T_end, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c, nfft){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degres with zero south, east negative, and west positive.")
  }
  
  # Step 1: Constrain T_start and T_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls, phi, T_start, T_end, beta, gamma_c)
  
  # No solar irradiance within the contrained time range.
  if(is.null(T_range)){
    return(0)
    
  }else{
    # Constrain the time range.
    T_start = T_range$T_start
    T_end = T_range$T_end
  }
  
  # Step 2: Calculate insolation.

  interand = function(T_s){
    G_di = Gdi_eq(Ls=Ls, phi=phi, longitude=longitude, T_s=T_s, tau=tau, al=al, beta=beta, nfft=nfft)
    return(G_di)
  }
  
  I_di = integrate(interand, T_start, T_end)
  
  # Return integration result.
  return(I_di$value)
}
