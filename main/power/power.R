# Load libraries.
library(here)
library(wesanderson)

# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
Gh_eq = dget(here("functions", "G_h.R"))

# Global hourly insolation on Mars horizontal surface [W/m2-h].
Ih_eq = dget(here("functions", "I_h.R"))

# Daylight range.
daylight_range = dget(here("utils", "daylight_range.R"))


# Calculate the generated energy [Wh] given the solar panel area [m2] and global insolation [Wh/m2].
#   A   - Solar panel area [m2].
#   Ih  - Global insolation on Mars horizontal surface [Wh/m2].
energy = function(A, Ih){
  E = A * e * Ih * PR
}

# Calculate the output power [W] given the solar panel area [m2] and global irradiance [W/m2].
#   A   - Solar panel area [m2].
#   Gh  - Global irradiance on Mars horizontal surface [W/m2].
power = function(A, Gh){
  P = A * e * Gh * PR
}

# Build the irradiance, energy, and power profiles.
get_profiles = function(A, Ls, phi, tau, nfft=3, T_step, verbose=FALSE){
  
  # Get the daylight solar time range for the Ls.
  Ts_range = daylight_range(Ls=Ls, phi=phi, T_step=T_step) 
  
  # Calculated generated solar panel energy.
  Gh_profile = c()
  E_profile = c()
  E_profile_cumulative = c()
  P_profile = c()
  
  E_prev = 0
  for(T_s in Ts_range){
    # Global irradiance on Mars horizontal surface [W/m2].
    Gh = Gh_eq(Ls=Ls, phi=phi, T_s=T_s, tau=tau, al=0.1, nfft=nfft)
    
    # Global hourly insolation on Mars horizontal surface [Wh/m2].
    Ih = Ih_eq(Ls=Ls, phi=phi, tau=tau, T_start=T_s, T_end=(T_s+T_step), al=0.1, nfft=nfft)
    
    # Power out from solar panels [Wh].
    P = power(A, Gh)
    
    # Energy generated from solar panels [Wh].
    E = energy(A, Ih)
    
    # Append calculated values to their respective vectors.
    P_profile = c(P_profile, P)
    
    E_profile = c(E_profile, E)
    
    E_profile_cumulative = c(E_profile_cumulative, E+E_prev)
    E_prev = E + E_prev
  
    Gh_profile = c(Gh_profile, Gh)
  }
  
  return(list(
    'Gh' = Gh_profile,
    'P'  = P_profile,
    'E'  = E_profile,
    'Ec' = E_profile_cumulative,
    'Ts' = Ts_range)
  )
}

Ls = 81       # Areocentric longitude.
phi = -2.05   # Planetary latitude of Victoria crater.
tau = 1       # Atmospheric opacity.

A = 1.2     # Solar array area.
e = 0.29    # Solar panel efficiency.
PR = 1 - (0.03 + 0.05 + 0.30) # Solar array performance ratio.

# Get insolation values.
T_step = 1 # hours.
profiles = get_profiles(A=A, Ls=Ls, phi=phi, tau=tau, nfft=3,
                               T_step=T_step,
                               verbose=FALSE)

dev.new()
plot(x=profiles$Ts,
     y=profiles$Gh,
     xlab="Solar Time, T [h]",
     ylab="Global Irradiance [W/m2]",
     type="l",
     lwd=4,
     col='grey',
     main='Global Irradiance on Mars Horizontal Surface')


dev.new()
plot(x=profiles$Ts,
     y=profiles$Ec,
     xlab="Solar Time, T [h]",
     ylab="Energy [Wh]",
     type="l",
     lwd=4,
     col='grey',
     main='Energy Collected')

dev.new()
barplot(profiles$E,
        names.arg=seq(round(profile$Ts[1]), round(profile$Ts[length(profile$Ts)]), T_step),
        xlab="Solar Time, T [h]",
        ylab="Energy [Wh]",
        cex.names=0.7,
        main='Energy Collected - Hourly')

dev.new()
plot(x=profiles$Ts,
     y=profiles$P,
     xlab="Solar Time, T [h]",
     ylab="Power [W]",
     type="l",
     lwd=4,
     col='grey',
     main='Solar Array Power Output')