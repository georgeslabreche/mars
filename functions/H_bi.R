library(here)

Ibi_eq = dget(here("functions", "I_bi.R"))

function(Ls, phi, tau, beta, gamma_c, nfft){
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
  }
  
  # H_bi is obtained by integrating I_bi over the period from sunrise to sunset.
  H_bi = Ibi_eq(Ls=Ls, phi=phi, tau=tau, T_start=0, T_end=24, beta=beta, gamma_c=gamma_c, nfft=nfft)
  
  # Return result.
  return(H_bi)
}