library(here)
library(wesanderson)

# Plot function.
tau_eq = dget(here("functions", "tau.R"))

mer_tau_lines = dget(here("functions/plots", "mer_tau_lines.R"))
vl_tau_lines = dget(here("functions/plots", "vl_tau_lines.R"))

Ls_seq = seq(0, 360, 1) # [deg]

# Last contact locations of missions.
phis = list(
  "Viking 1" = 22.27,
  "Viking 2" =  47.64,
  "Spirit" = -14.6,
  "Opportunity" = -2.28)
#  "Phoenix" = 68.22)

cols = wes_palette("FantasticFox1", 10, type="continuous")

# main="Optical Depth as a Function of Areocentric Longitude",
dev.new()
par(mfrow=c(2,2))

mission_index = 1
for(phi in phis){

  # Get tau values by applying both analytical expression models.
  tau_seq_model_1 = c()
  for(Ls in Ls_seq){
    tau = tau_eq(phi, Ls, 1)
    tau_seq_model_1 = c(tau_seq_model_1, tau)
  }
  
  tau_seq_model_2 = c()
  for(Ls in Ls_seq){
    tau = tau_eq(phi, Ls, 2)
    tau_seq_model_2 = c(tau_seq_model_2, tau)
  }
  
  #########################################################
  # Plot tau values determiend by analytical expressions. #
  #########################################################
  plot(Ls_seq, tau_seq_model_1,
       xlab="Ls [deg]",
       ylab="τ",
       ylim=c(0, 7),
       type="l",
       lty=2,
       sub=paste(names(phis)[mission_index], " (ϕ = ", phi, "°)", sep=""),
       font.sub=2,
       cex.sub=1.2,
       col="red")
  
  lines(Ls_seq, tau_seq_model_2,
        type="l",
        lty=2,
        col="blue")
  
  # Legend
  if(mission_index == 1){
    legend("topleft",
           c("Model 1 (ϕ₁=-30°, ϕ₂=-30°)", "Model 2 (ϕ₁=-30°, ϕ₂=-10°)"),		           
           col=c("red", "blue"),
           cex=1, bty="n", lty=2)
  }
  
  
  
  ######################################################
  # Plot tau ground measurements for selected mission. #
  ######################################################
  if(phi == phis$`Viking 1`){
    vl_tau_lines("VL1", cols=cols)
  
  }else if(phi == phis$`Viking 2`){
    vl_tau_lines("VL2", cols=cols)
    
  }else if(phi == phis$Spirit){
    mer_tau_lines("Spirit", cols=cols)
    
  }else if(phi == phis$Opportunity){
    mer_tau_lines("Oppy", cols=cols)
    
  }else{
    stop("Unsupported mission selected.")
  }
  
  mission_index = mission_index + 1
}


mtext(paste("Optical Depth as a Function of Areocentric Longitude\nGlobal Dust Storm Analytical Models vs Ground Measurements",  sep=""), side = 3, line = -3, outer = TRUE)



