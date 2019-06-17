# Global irradiance on an inclined surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994). Solar radiation on Mars: Tracking photovoltaic array. Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://www.researchgate.net/publication/24286713_Solar_radiation_on_Mars_Tracking_photovoltaic_array

# Clear Global environment variables.
rm(list = ls())

# Load libraries.
library(here)
library(wesanderson)
library(imager)

Gbeta_eq = dget(here("functions", "G_beta.R"))

Z_eq = dget(here("functions", "Z.R"))

gamma_orientation_angles = list(
  "South" = 0,
  "Southeast" = 45,
  "East" = 90,
  "Northeast" = 135,
  "North" = 180,
  "Northwest" = 225,
  "West" = 270,
  "Southwest" = 315)

tau = 0     # Optical depth.
al = 0.1    # Albedo.
nfft = 3    # Net flux function type.

###################################################################
# Default Ls and phi values based on past mission landing sites. #
##################################################################
# MER-A Spirit Landing Site
# January 4, 2004, 04:35 UTC 
Ls_spirit = 327.66536 # https://jtauber.github.io/mars-clock/
phi_spirit = -14.7542
omega_spirit = 15.5786 # https://www.calculatorsoup.com/calculators/time/time-to-decimal-calculator.php

# MER-B Oppy Landing Site
# January 25, 2004, 05:05 UTC
Ls_oppy = 339.10504 # https://jtauber.github.io/mars-clock/
phi_oppy = -1.9483
omega_oppy = 14.5803 # https://www.calculatorsoup.com/calculators/time/time-to-decimal-calculator.php

# Selected default values.
Ls = Ls_oppy
phi = phi_oppy 

####################################################################################
# Title: Variation of Mars solar irradiance Gβ for different slope orientations γ. #
# Subtitle: Effect of hour angle ω with slope angle β as a parameter.              #
####################################################################################
dev.new()
par(mfrow=c(3,4))

Ls = 75 # Areocentric longitude [deg].
omegas = 7:17 # Solar time [h].

# FIXME: Solar time 12h throws aNaNs produced error for a acos((x - y)/z) in the G_beta function.
# Removing that solar time  value until it is resolved.
omegas = omegas[!omegas %in% 12]

gamma_index = 1
for(gamma_c in gamma_orientation_angles){
  beta_index = 1

  betas = seq(0, 90, 15)
  for(beta in betas){
    
    y = c()
    for(omega in omegas){
      G = Gbeta_eq(Ls, omega, phi, tau, al, beta, gamma_c, nfft)
      y = c(y, G)
    }
    
    if(beta_index == 1){
      plot(omegas, y,
            xlab="ω [h]", ylab="Gβ [W/m2]",
            sub=paste("γ = ", gamma_c, "°", sep=""),
            font.sub=2,
            cex.sub=1.2,
            type="l",
            col=wes_palette("FantasticFox1", length(betas), type = "continuous")[beta_index])
    
    }else{
      lines(omegas, y,
           xlab="ω [h]", ylab="Gβ [W/m2]",
           sub=paste("γ = ", gamma_c, "°", sep=""),
           font.sub=2,
           cex.sub=1.2,
           type="l",
           col=wes_palette("FantasticFox1", length(betas), type = "continuous")[beta_index])
    }

    beta_index = beta_index + 1
  }

  gamma_index = gamma_index + 1
}

plot.new()
legend("top",
       paste("β = ", betas, "°" , sep=""),
       col=wes_palette("FantasticFox1", length(betas), type="continuous"),
       cex=0.8, bty="n", lty=1)

mtext(paste("Variation of Mars solar irradiance Gβ for different slope orientations γ \nEffect of the hour angle ω with slope angle β as a parameter (Ls=", round(Ls), "°, ϕ=", phi, "°, τ=", tau, ", al=", al, ")", sep=""), side=3, line=-3, outer=TRUE)

#############################################################################
# Title: Variation of Mars solar irradiance Gβ for different slope angle β. #
# Subtitle: Effect of Sun zenith angle Z with latitude ϕ as a parameter.    #
#############################################################################
dev.new()
par(mfrow=c(2,4))

phis = seq(-75, 75, 15) # Planetary Latitudes [deg].

# FIXME: Solar times throw a NaNs produced error for a acos((x - y)/z) in the G_beta function.
omegas = 0:24 # Solar time [h].

betas = seq(0, 90, 15) # Slope Angles [deg].
gamma_c = gamma_orientation_angles$South # The slope orientation [deg].

beta_index = 1
for(beta in betas){

  phi_index = 1
  for(phi in phis){

    omega_index = 1
    x = c()
    y = c()
    for(omega in omegas){

      Z = Z_eq(Ls, omega, phi, nfft)
      x = c(x, Z)

      G = Gbeta_eq(Ls, omega, phi, tau, al, beta, gamma_c, nfft)
      y = c(y, G)

      omega_index = omega_index + 1
    }

    if(phi_index == 1){
      plot(x, y,
           xlab="Z [deg]", ylab="Gβ [W/m2]",
           xlim=c(0, 75), ylim=c(0, 500),
           type="l",
           sub=paste("β = ", beta, "°", sep=""),
           font.sub=2,
           cex.sub=1.2,
           col=wes_palette("FantasticFox1", length(phis), type="continuous")[phi_index])

    }else{
      lines(x, y,
            type="l",
            sub=paste("β = ", beta, "°", sep=""),
            font.sub=2,
            cex.sub=1.2,
            col=wes_palette("FantasticFox1", length(phis), type="continuous")[phi_index])
    }

    phi_index = phi_index + 1
  }

  beta_index = beta_index + 1
}

plot.new()
legend("topleft",
        paste("ϕ = ", phis, "°" , sep=""),
        col=wes_palette("FantasticFox1", length(phis), type="continuous"),
        cex=0.8, bty="n", lty=1)

mtext(paste("Variation of Mars solar irradiance Gβ for different slope angle β\nEffect of Sun zenith angle Z with latitude ϕ as a parameter (Ls=", round(Ls), "°, γ=", gamma_c, "°, τ=", tau, ", al=", al, ")", sep=""), side=3, line=-3, outer=TRUE)


####################################################################################
# Title: Variation of Mars solar irradiance Gβ for different slope orientations γ. #
# Subtitle: Effect of Sun zenith angle Z with latitude ϕ as a parameter.           #
####################################################################################
dev.new()
par(mfrow=c(3,4))

phis = seq(-75, 75, 15) # Planetary Latitudes [deg].

# FIXME: Solar times throw a NaNs produced error for a acos((x - y)/z) in the G_beta function.
omegas = 0:24 # Solar time [h].

beta = 45 # Slope Angle [deg].

gamma_index = 1
for(gamma_c in gamma_orientation_angles){

  phi_index = 1
  for(phi in phis){

    omega_index = 1
    x = c()
    y = c()
    for(omega in omegas){

      Z = Z_eq(Ls, omega, phi, nfft)
      x = c(x, Z)

      G = Gbeta_eq(Ls, omega, phi, tau, al, beta, gamma_c, nfft)
      y = c(y, G)

      omega_index = omega_index + 1
    }

    if(phi_index == 1){
      plot(x, y,
           xlab="Z [deg]", ylab="Gβ [W/m2]",
           xlim=c(0, 75), ylim=c(0, 500),
           type="l",
           sub=paste("γ = ", gamma_c, "°", sep=""),
           font.sub=2,
           cex.sub=1.2,
           col=wes_palette("FantasticFox1", length(phis), type="continuous")[phi_index])

    }else{
      lines(x, y,
            type="l",
            sub=paste("γ = ", gamma_c, "°", sep=""),
            font.sub=2,
            cex.sub=1.2,
            col=wes_palette("FantasticFox1", length(phis), type="continuous")[phi_index])
    }

    phi_index = phi_index + 1
  }

  gamma_index = gamma_index + 1
}

plot.new()
legend("topleft",
       paste("ϕ = ", phis, "°" , sep=""),
       col=wes_palette("FantasticFox1", length(phis), type="continuous"),
       cex=0.8, bty="n", lty=1)

mtext(paste("Variation of Mars solar irradiance Gβ for different slope orientations γ\nEffect of Sun zenith angle Z with latitude ϕ as a parameter (Ls=", round(Ls), "°, β=", beta, "°, τ=", tau, ", al=", al, ")", sep=""), side=3, line=-3, outer=TRUE)

####################################################################################

dev.new()
im <- load.image(here("images", "angles.png"))
plot(im, axes=FALSE,
     main="Zenith angle, surface tilt, surface azimuth angle, latitude, and\nlocal meridian for a tilted surface (Widén 2010)")