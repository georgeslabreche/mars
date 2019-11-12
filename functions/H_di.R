library(here)

Idi_eq = dget(here("functions", "I_di.R"))

source(here("functions", "albedo.R"))

function(Ls, phi, longitude, tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, gamma_c, nfft){
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
  }
  
  # H_di is obtained by integrating I_di over the period from sunrise to sunset.
  H_di = Idi_eq(Ls=Ls, phi=phi, longitude=longitude, tau=tau, T_start=0, T_end=24, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
  
  # Return result.
  return(H_di)
}