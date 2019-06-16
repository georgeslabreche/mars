# Global irradiance on an inclined surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994). Solar radiation on Mars: Tracking photovoltaic array. Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://www.researchgate.net/publication/24286713_Solar_radiation_on_Mars_Tracking_photovoltaic_array

# Load libraries.
library(wesanderson)
library(imager)

Gbeta_eq = dget(here("functions", "G_beta.R"))

# MER-B Oppy Landing Site
# January 25, 2004, 05:05 UTC
Ls = 339.10504 # https://jtauber.github.io/mars-clock/
omega = 14.5803 # https://www.calculatorsoup.com/calculators/time/time-to-decimal-calculator.php
phi = -1.9483


tau = 0.5
al = 0.1
# If using the albedo function, the longitude and latitude (phi) must be a multiple of 10.
#l = albedo(0, phi)
beta = 0
gamma_c = 0 # The rover is oriented soutwards [deg].
nfft = 3

dev.new()
par(mfrow=c(3,4))

gamma_index = 1
gammas = seq(0, 360, 45)
for(gamma_c in gammas){
  beta_index = 1
  
  betas = seq(0, 90, 15)
  for(beta in betas){
    diurnal_f <- function(omega){
      Gbeta_eq(Ls, omega, phi, tau, al, beta, gamma_c, nfft)
    }
    
    curve(diurnal_f, from=7, to=17, n=10,
          xlab="ω [h]", ylab="Gh [W/m2]",
          add=(if(beta_index == 1) FALSE else TRUE),
          sub=paste("γ = ", gamma_c, "°", sep=""),
          font.sub=2,
          cex.sub=1.2,
          col=wes_palette("FantasticFox1", length(betas), type = "continuous")[beta_index])
    
    beta_index = beta_index + 1
  }
  
  gamma_index = gamma_index + 1
}

plot.new()
legend("top",
       paste("β = ", betas, "°" , sep=""),
       col=wes_palette("FantasticFox1", length(betas), type="continuous"),
       cex=0.8, bty="n", lty=1)

mtext(paste("Effect of Orientation and Slope Angleon Mars Solar Irradiance\nLs=", round(Ls), "°, ϕ=", phi, "°, τ=", tau, ", al=", al, sep=""), side=3, line=-3, outer=TRUE)


#dev.new()
#im <- load.image(here("images", "angles.png"))
#plot(im, axes=FALSE)