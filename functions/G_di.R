library(here)

# Equation 6 (1990): Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Equation 16 (1990): Diffuse irrafiance on a horizontal Martian surface [W/m2].
Gdh_eq = dget(here("functions", "G_dh.R"))

source(here("functions", "albedo.R"))

function(Ls, phi, longitude, T_s, Z=Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft), tau, al=albedo(latitude=phi, longitude=longitude, tau=tau), beta, nfft){
  G_di = Gdh_eq(Ls=Ls, phi=phi, longitude=longitude, Z=Z, tau=tau, al=al, nfft=nfft) * cos((beta*pi/180) / 2)^2
  return(G_di)
}