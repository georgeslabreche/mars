library(here)

Ib_beta_eq = dget(here("functions", "I_b_beta.R"))

function(Ls, phi, tau, al=0.1, beta, gamma_c, nfft){
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
  }
  
  # H_b_beta is obtained by integrating I_b_beta over the period from sunrise to sunset.
  H_b_beta = Ib_beta_eq(Ls=Ls, phi=phi, tau=tau, T_start=0, T_end=24, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
  
  # Return result.
  return(H_b_beta)
}