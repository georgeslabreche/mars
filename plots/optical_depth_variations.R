# Load
library(here)
library(wesanderson)

# Plot function.
tau_eq = dget(here("functions", "tau.R"))

#################################################################################
# Variation of optical depth with areocentric longitude at different latitudes. #
#################################################################################
phi_seq = seq(-30, 30, 10) # [deg]
Ls_seq = seq(0, 360, 1) # [deg]

dev.new()
phi_index = 1
for(phi in phi_seq){
  tau_seq = c()

  # For some reason not working when doing tau_eq(phi, Ls_seq, 1).
  # So get taus iteratively.
  for(Ls in Ls_seq){
    tau = tau_eq(phi, Ls, 1)
    tau_seq = c(tau_seq, tau)
  }

  if(phi_index == 1){
    plot(Ls_seq, tau_seq,
         xlab="Ls [deg]", ylab="τ",
         font.sub=2,
         cex.sub=1.2,
         type='l',
         col=wes_palette("FantasticFox1", length(phi_seq), type="continuous")[phi_index],
         main="Variation of optical depth with areocentric longitude\nat different latitudes")
  }else{
    lines(Ls_seq, tau_seq, type='l',
          col=wes_palette("FantasticFox1", length(phi_seq), type="continuous")[phi_index])
  }

  phi_index = phi_index + 1

}

legend("topleft", ,
       paste("ϕ = ", phi_seq, "°", sep=""),
       col=wes_palette("FantasticFox1", length(phi_seq), type="continuous"),
       cex=0.8, bty="n", lty=1)

##################################################################################
# Variation of optical depth with latitudes at different areocentric longitudes. #
##################################################################################
phi_seq = seq(-60, 60, 1) # [deg]

Ls_list = list(
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
Ls_index = 1
for(Ls in rev(Ls_seq)){
  tau_seq = c()
  
  # For some reason not working when doing tau_eq(phi_seq, Ls, 1).
  # So get taus iteratively.
  for(phi in phi_seq){
    tau = tau_eq(phi, Ls, 1)
    tau_seq = c(tau_seq, tau)
  }
  
  if(Ls_index == 1){
    plot(phi_seq, tau_seq,
         xlab="ϕ [deg]", ylab="τ",
         ylim=c(0, 7),
         font.sub=2,
         cex.sub=1.2,
         type='l',
         col=wes_palette("FantasticFox1", length(Ls_seq), type="continuous")[Ls_index],
         main="Variation of optical depth with latitudes\nat different areocentric longitudes")
  }else{
    lines(phi_seq, tau_seq, type='l',
          col=wes_palette("FantasticFox1", length(Ls_seq), type="continuous")[Ls_index])
  }
  
  Ls_index = Ls_index + 1
  
}

legend("topright",
       paste(rev(names(Ls_list)), sep=""),
       col=wes_palette("FantasticFox1", length(Ls_list), type="continuous"),
       cex=0.8, bty="n", lty=1)