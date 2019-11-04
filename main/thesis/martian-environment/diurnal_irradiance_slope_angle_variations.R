# Global irradiance on an inclined surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994). Solar radiation on Mars: Tracking photovoltaic array. Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://www.researchgate.net/publication/24286713_Solar_radiation_on_Mars_Tracking_photovoltaic_array

# Load libraries.
library(here)
library(wesanderson)
library(imager)

config_filepath = here('main', 'config.yml')
config = read.config(file=config_filepath)

Gbeta_eq = dget(here("functions", "G_beta.R"))
Z_eq = dget(here("functions", "Z.R"))

gamma_orientation_angles = list(
  "South" = 0,
  "East" = -90,
  "North" = -180,
  "West" = -270)

Ls = 248 # Periphelion.
phi = -2 # Endeavour crater.
tau = 0.5 # Optical depth.
al = 0.1  # Albedo.
nfft = 3  # Net flux function type.

ylim = c(0, 620)

####################################################################################
# Title: Variation of Mars solar irradiance Gβ for different slope orientations γ. #
# Subtitle: Effect of solar time with slope angle β as a parameter.                #
####################################################################################
plot_a = function(Ls, phi, tau, al, Ts_range, betas, gammas, xTs=TRUE){
  
  gamma_c_index = 1
  
  for(gamma_c in gammas){
    
    dev.new()
    par(bg='white')
    
    beta_index = 1
    
    betas = seq(0, 80, 20)
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
        
        G = Gbeta_eq(Ls=Ls, T_s=T_s, phi=phi, tau=tau, al=al, beta=beta, gamma_c=gamma_c, nfft=nfft)
        y = c(y, G)
      }
      
      if(beta_index == 1){
        plot(x, y,
             xlab=if(isTRUE(xTs)) "Solar Time [h]" else "Z [deg]",
             ylab="Gβ [W/m2]",
             xlim=if(isTRUE(xTs)) c(Ts_range[1], Ts_range[length(Ts_range)]) else c(0, 75),
             ylim=ylim,
             #sub=paste("γ = ", gamma_c, "°", sep=""),
             font.sub=2,
             cex.sub=1.2,
             type="l",
             lwd=3,
             col=wes_palette("FantasticFox1", length(betas), type = "continuous")[beta_index])
        
        if(gamma_c_index == 1){
          legend("topleft",
                 title="β",
                 paste( betas, "°" , sep=""),
                 col=wes_palette("FantasticFox1", length(betas), type="continuous"),
                 cex=1, lty=1, lwd=3)
        }
        
      }else{
        lines(x, y,
              type="l",
              col=wes_palette("FantasticFox1", length(betas), type="continuous")[beta_index],
              lwd=3)
      }
      
      beta_index = beta_index + 1
    }
    
    title_template = "Diurnal irradiance slope angle variation {{index}} for Ls {{Ls}}, phi {{phi}}, tau {{tau}}, and gamma_c {{gamma_c}}."
    title_data = list(index=gamma_c_index, Ls=Ls, phi=phi, tau=tau, gamma_c=gamma_c)
    title = whisker.render(title_template, title_data)
    
    plot_filepath = paste(config$output$marsenv, slugify(title, space_replace="-"), ".png", sep="")
    dev.copy(png, plot_filepath)
    dev.off()
    
    gamma_c_index = gamma_c_index + 1
    
  }
}


#######
# Plot effect of solar time.
#######
# Set some parameters before plotting.
Ts_range = 5:20 # Solar time [h].
Ts_range = seq(5, 20, 0.05)

# FIXME: Solar time 12h throws a NaNs produced error for a acos((x - y)/z) in the G_beta function.
# Removing that solar time  value until it is resolved.
Ts_range = Ts_range[!Ts_range %in% 12]

# Plot.
plot_a(Ls=Ls, phi=phi, tau=tau, al=al, Ts_range=Ts_range, betas=betas,
       gammas=gamma_orientation_angles, xTs=TRUE)

