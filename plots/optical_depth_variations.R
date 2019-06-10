# Load
library(here)
library(wesanderson)

# Plot function.
tau_eq = dget(here("functions", "tau.R"))

# tau expresson models.
models = c(1, 2)

Ls_opportunity_last_contact = 190
phi_opportunity_last_contact = -2.28

#################################################################################
# Variation of optical depth with areocentric longitude at different latitudes. #
#################################################################################
phi_seq = c(seq(-30, 30, 10), phi_opportunity_last_contact) # [deg]
Ls_seq = seq(0, 360, 1) # [deg]

dev.new()
par(mfrow=c(1,2))
for(model in models){
  
  phi_index = 1
  for(phi in phi_seq){
    
    # Emphasize opportunity last contact case.
    lty = if(phi==phi_opportunity_last_contact) 2 else 1
    
    tau_seq = c()
  
    # For some reason not working when doing tau_eq(phi, Ls_seq, 1).
    # So get taus iteratively.
    for(Ls in Ls_seq){
      tau = tau_eq(phi, Ls, model)
      tau_seq = c(tau_seq, tau)
    }
  
    if(phi_index == 1){
      plot(Ls_seq, tau_seq,
           xlab="Areocentric Longitude, Ls [deg]",
           ylab="Optical Depth, τ",
           ylim=c(0, 7),
           sub=paste("Model ", model),
           font.sub=2,
           cex.sub=1.2,
           type="l",
           lty=lty,
           col=wes_palette("FantasticFox1", length(phi_seq), type="continuous")[phi_index])
    }else{
      lines(Ls_seq, tau_seq, type='l', lty=lty,
            col=wes_palette("FantasticFox1", length(phi_seq), type="continuous")[phi_index])
    }
  
    phi_index = phi_index + 1
  
  }
  
  tau = tau_eq(phi_opportunity_last_contact, Ls_opportunity_last_contact, model)
  
  points(Ls_opportunity_last_contact, tau,
         col=wes_palette("FantasticFox1", length(phi_seq), type="continuous")[phi_index-1])
  
  text(Ls_opportunity_last_contact, tau,
       labels=paste("Opportunity Last Contact\nLs=", Ls_opportunity_last_contact, "°, ϕ= ", phi_opportunity_last_contact, "°\nPredicted τ=", round(tau, 2), ", Actual τ=", 10.8, sep=""),
       cex=0.5,
       pos=2)
  
  legend("topleft",
         paste("ϕ = ", phi_seq, "°", sep=""),
         col=wes_palette("FantasticFox1", length(phi_seq), type="continuous"),
         cex=0.8, bty="n", lty=1)
}

mtext("Variation of optical depth with areocentric longitude at different latitudes", side=3, line=-3, outer=TRUE)


##################################################################################
# Variation of optical depth with latitudes at different areocentric longitudes. #
##################################################################################
phi_seq = seq(-90, 90, 10)  # [deg]

Ls_list = list(
  "Ls = 190° (Opportunity Last Contact)" = Ls_opportunity_last_contact,
  #"Ls = 0°   (Vernal Equinox) " = 0,
  "Ls = 71°   (Aphelion)" = 71,
  #"Ls = 90°  (Summer Solstice)" = 90,
  "Ls = 180° (Autumn Equinox)" = 180,
  "Ls = 215°" = 215,
  "Ls = 248° (Periphelion)" = 248,
  "Ls = 270° (Winter Solstice)" = 270,
  "Ls = 295°" = 295)

Ls_seq = unlist(Ls_list, use.names=FALSE)

dev.new()
par(mfrow=c(1,2))
for(model in models){
    
  Ls_index = 1
  for(Ls in rev(Ls_seq)){
    tau_seq = c()
    
    # Emphasize opportunity last contact case.
    lty = if(Ls==Ls_opportunity_last_contact) 2 else 1
    
    
    # For some reason not working when doing tau_eq(phi_seq, Ls, 1).
    # So get taus iteratively.
    for(phi in phi_seq){
      tau = tau_eq(phi, Ls, model)
      tau_seq = c(tau_seq, tau)
    }
    
    if(Ls_index == 1){
      plot(phi_seq, tau_seq,
           xlab="Latitude, ϕ [deg]",
           ylab="Optical Depth, τ",
           xaxt='n',
           yaxt='n',
           ylim=c(0.5, 8),
           sub=paste("Model ", model),
           font.sub=2,
           cex.sub=1.2,
           type='l',
           lty=lty,
           col=wes_palette("FantasticFox1", length(Ls_seq), type="continuous")[Ls_index])
      
      axis(1, seq(-90, 90, 10))
      axis(2, seq(0.5, 8, .5))
    }else{
      lines(phi_seq, tau_seq,
            yaxt='n',
            type='l',
            lty=lty,
            col=wes_palette("FantasticFox1", length(Ls_seq), type="continuous")[Ls_index])
    }
    
    Ls_index = Ls_index + 1
    
  }
  
  tau = tau_eq(phi_opportunity_last_contact, Ls_opportunity_last_contact, model)
  
  points(phi_opportunity_last_contact, tau,
         col=wes_palette("FantasticFox1", length(Ls_seq), type="continuous")[Ls_index-1])
  
  text(phi_opportunity_last_contact, tau,
       labels=paste("Opportunity Last Contact\nLs=", Ls_opportunity_last_contact, "°, ϕ= ", phi_opportunity_last_contact, "°\nPredicted τ=", round(tau, 2), ", Actual τ=", 10.8, sep=""),
       cex=0.5,
       pos=4)
  
  legend("topright",
         paste(rev(names(Ls_list)), sep=""),
         col=wes_palette("FantasticFox1", length(Ls_list), type="continuous"),
         cex=0.7, bty="n", lty=1)
}
mtext("Variation of optical depth with latitudes at different areocentric longitudes", side=3, line=-3, outer=TRUE)

