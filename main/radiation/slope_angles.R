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

Gh_beta_eq = dget(here("functions", "G_h_beta.R"))

Z_eq = dget(here("functions", "Z.R"))

gamma_orientation_angles = list(
  "South" = 0,
  "Southeast" = -45,
  "East" = -90,
  "Northeast" = -135,
  "North" = 180,
  "Northwest" = 135,
  "West" = 90,
  "Southwest" = 45)

tau = 0   # Optical depth.
al = 0.1  # Albedo.
nfft = 3  # Net flux function type.

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
# Subtitle: Effect of solar time with slope angle β as a parameter.                #
####################################################################################
plot_a = function(Ls, phi, tau, al, Ts_range, betas, gammas, xTs=TRUE){
  dev.new()
  par(mfrow=c(3,4))

  for(gamma_c in gammas){
    beta_index = 1
    
    betas = seq(0, 90, 15)
    for(beta in betas){
      
      x = c()
      y = c()
      for(T_s in Ts_range){
        if(isTRUE(xTs)){
          x = c(x, T_s)
        }else{
          Z = Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft)
          x = c(x, Z)
        }
        
        G = Gh_beta_eq(Ls=Ls, phi=phi, T_s=T_s, tau=tau, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
        y = c(y, G)
      }
      
      if(beta_index == 1){
        plot(x, y,
             xlab=if(isTRUE(xTs)) "solar time [h]" else "Z [deg]",
             ylab="Gβ [W/m2]",
             xlim=if(isTRUE(xTs)) c(Ts_range[1], Ts_range[length(Ts_range)]) else c(0, 75),
             ylim=c(0, 500),
             sub=paste("γ = ", gamma_c, "°", sep=""),
             font.sub=2,
             cex.sub=1.2,
             type="l",
             col=wes_palette("FantasticFox1", length(betas), type = "continuous")[beta_index])
        
      }else{
        lines(x, y,
              type="l",
              col=wes_palette("FantasticFox1", length(betas), type = "continuous")[beta_index])
      }
      
      beta_index = beta_index + 1
    }
    
  }
  
  plot.new()
  legend("top",
         paste("β = ", betas, "°" , sep=""),
         col=wes_palette("FantasticFox1", length(betas), type="continuous"),
         cex=0.8, bty="n", lty=1)
  
  effect = if(isTRUE(xTs)) "solar time" else "Sun zenith angle Z"
  title = paste("Variation of Mars solar irradiance Gβ for different slope orientations γ \nEffect of ", effect, " with slope angle β as a parameter (Ls=", round(Ls), "°, ϕ=", phi, "°, τ=", tau, ", al=", al, ")", sep="")
  mtext(title, side=3, line=-3, outer=TRUE)
}


#######
# Plot effect of solar time.
#######
# Set some parameters before plotting.
Ts_range = 7:17 # Solar time [h].

# FIXME: Solar time 12h throws aNaNs produced error for a acos((x - y)/z) in the G_beta function.
# Removing that solar time  value until it is resolved.
Ts_range = Ts_range[!Ts_range %in% 12]

# Plot.
plot_a(Ls=Ls, phi=phi, tau=tau, al=al, Ts_range=Ts_range, betas=betas,
       gammas=gamma_orientation_angles, xTs=TRUE)

#######
# Plot effect of Sun zenith angle Z.
#######
# Set some parameters before plotting.
omegas = 0:24 # Solar time [h].

# Plot.
plot_a(Ls=Ls, phi=phi, tau=tau, al=al, Ts_range=Ts_range, betas=betas,
       gammas=gamma_orientation_angles, xTs=FALSE)

#############################################################################
# Title: Variation of Mars solar irradiance Gβ for different slope angle β. #
# Subtitle: Effect of solar angle ω with latitude ϕ as a parameter.         #
#############################################################################
plot_b = function(Ls, phis, tau, al, Ts_range, betas, gamma_c, xTs=TRUE){
  dev.new()
  par(mfrow=c(2,4))

  for(beta in betas){

    phi_index = 1
    for(phi in phis){

      x = c()
      y = c()
      for(T_s in Ts_range){

        if(isTRUE(xTs)){
          x = c(x, T_s)
        }else{
          Z = Z_eq(Ls, T_s, phi, nfft)
          x = c(x, Z)
        }

        G = Gh_beta_eq(Ls=Ls, phi=phi, T_s=T_s, tau=tau, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
        y = c(y, G)
      }

      if(phi_index == 1){
        plot(x, y,
             xlab=if(isTRUE(xTs)) "solar time [h]" else "Z [deg]",
             ylab="Gβ [W/m2]",
             xlim=if(isTRUE(xTs)) c(Ts_range[1], Ts_range[length(Ts_range)]) else c(0, 75),
             ylim=c(0, 500),
             type="l",
             sub=paste("β = ", beta, "°", sep=""),
             font.sub=2,
             cex.sub=1.2,
             col=wes_palette("FantasticFox1", length(phis), type="continuous")[phi_index])

      }else{
        lines(x, y,
              type="l",
              col=wes_palette("FantasticFox1", length(phis), type="continuous")[phi_index])
      }

      phi_index = phi_index + 1
    }
  }

  plot.new()
  legend("topleft",
         paste("ϕ = ", phis, "°" , sep=""),
         col=wes_palette("FantasticFox1", length(phis), type="continuous"),
         cex=0.8, bty="n", lty=1)

  effect = if(isTRUE(xTs)) "solar time" else "Sun zenith angle Z"
  title = paste("Variation of Mars solar irradiance Gβ for different slope angle β\nEffect of ", effect, " with latitude ϕ as a parameter (Ls=", round(Ls), "°, γ=", gamma_c, "°, τ=", tau, ", al=", al, ")", sep="")
  mtext(title, side=3, line=-3, outer=TRUE)

}

# Set some parameters before plotting.
phis = seq(-75, 75, 15) # Planetary Latitudes [deg].
betas = seq(0, 90, 15) # Slope Angles [deg].
gamma_c = gamma_orientation_angles$South # The slope orientation [deg].

#######
# Plot effect of solar time.
#######
Ts_range = 7:18 # Solar time [h].
# FIXME: Solar time 12h throws aNaNs produced error for a acos((x - y)/z) in the G_beta function.
# Removing that solar time 12 value until it is resolved.
Ts_range = Ts_range[!Ts_range %in% 12]

# Plot.
plot_b(Ls, phis=phis, tau=tau, al=al, Ts_range=Ts_range,
       betas=betas, gamma_c=gamma_c, xTs=TRUE)

#######
# Plot effect of Sun zenith angle Z.
#######
Ts_range = 0:24 # Solar time [h].

# Plot.
plot_b(Ls=Ls, phis=phis, tau=tau, al=al, Ts_range=Ts_range,
       betas=betas, gamma_c=gamma_c, xTs=FALSE)


####################################################################################
# Title: Variation of Mars solar irradiance Gβ for different slope orientations γ. #
# Subtitle: Effect of solar time with latitude ϕ as a parameter.                   #
####################################################################################
plot_c = function(Ls, phis, tau, al, Ts_range, beta, gammas, xTs=TRUE){
  dev.new()
  par(mfrow=c(3,4))

  for(gamma_c in gammas){

    phi_index = 1
    for(phi in phis){

      x = c()
      y = c()
      for(T_s in Ts_range){

        if(isTRUE(xTs)){
          x = c(x, T_s)
        }else{
          Z = Z_eq(Ls, T_s, phi, nfft)
          x = c(x, Z)
        }

        G = Gh_beta_eq(Ls=Ls, phi=phi, T_s=T_s, tau=tau, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
        y = c(y, G)
      }

      if(phi_index == 1){
        plot(x, y,
             xlab=if(isTRUE(xTs)) "solar time [h]" else "Z [deg]",
             ylab="Gβ [W/m2]",
             xlim=if(isTRUE(xTs)) c(Ts_range[1], Ts_range[length(Ts_range)]) else c(0, 75),
             ylim=c(0, 500),
             type="l",
             sub=paste("γ = ", gamma_c, "°", sep=""),
             font.sub=2,
             cex.sub=1.2,
             col=wes_palette("FantasticFox1", length(phis), type="continuous")[phi_index])

      }else{
        lines(x, y,
              type="l",
              col=wes_palette("FantasticFox1", length(phis), type="continuous")[phi_index])
      }

      phi_index = phi_index + 1
    }
  }

  plot.new()
  legend("topleft",
         paste("ϕ = ", phis, "°" , sep=""),
         col=wes_palette("FantasticFox1", length(phis), type="continuous"),
         cex=0.8, bty="n", lty=1)

  effect = if(isTRUE(xTs)) "solar time" else "Sun zenith angle Z"
  title = paste("Variation of Mars solar irradiance Gβ for different slope orientations γ\nEffect of ", effect, " with latitude ϕ as a parameter (Ls=", round(Ls), "°, β=", beta, "°, τ=", tau, ", al=", al, ")", sep="")
  mtext(title, side=3, line=-3, outer=TRUE)

}

# Set some parameters before plotting.
phis = seq(-75, 75, 15) # Planetary Latitudes [deg].
beta = 45 # Slope Angle [deg].
gamma_c = gamma_orientation_angles$South # The slope orientation [deg].

#######
# Plot effect of solar time.
#######
Ts_range = 7:18 # Solar time [h].
# FIXME: Solar time 12h throws a NaNs produced error for a acos((x - y)/z) in the G_beta function.
# Removing that solar time 12 value until it is resolved.
Ts_range = Ts_range[!Ts_range %in% 12]

# Plot.
plot_c(Ls=Ls, phis=phis, tau=tau, al=al, Ts_range=Ts_range,
       beta=beta, gammas=gamma_orientation_angles, xTs=TRUE)

#######
# Plot effect of Sun zenith angle Z.
#######
Ts_range = 0:24 # Solar time [h].
# FIXME: Solar time 12h throws a NaNs produced error for a acos((x - y)/z) in the G_beta function.
# Removing that solar time 12 value until it is resolved.

# Plot.
plot_c(Ls=Ls, phis=phis, tau=tau, al=al, Ts_range=Ts_range,
       beta=beta, gammas=gamma_orientation_angles, xTs=FALSE)



####################################
# Plot angle figure as visual aid. #
####################################
dev.new()
im <- load.image(here("images", "solar_angles_on_an_inclined_surface.png"))
plot(im, axes=FALSE,
     main="Solar angles on an inclined surface (Appelbaum et al. 1993)")