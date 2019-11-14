library(here)

# Equation 6 (1990): Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
Gh_eq = dget(here("functions", "G_h.R"))

source(here("functions", "albedo.R"))

function(Ls, phi, longitude, T_s, Z=Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, nfft){
  Gali = al * Gh_eq(Ls=Ls, phi=phi, longitude=longitude, Z=Z, tau=tau, al=al, nfft=nfft) * sin((beta*pi/180) / 2)^2
  return(Gali)
}

