# Daily variation of global, beam, and diffuse insolation on Mars horizontal surface.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

# Load libraries
library(here)
library(wesanderson)
library(whisker)

Hh_eq = dget(here("functions", "H_h.R"))
Hbh_eq = dget(here("functions", "H_bh.R"))
Hdh_eq = dget(here("functions", "H_dh.R"))

H_eqs = c(Hh_eq, Hbh_eq, Hdh_eq)

# Legend labels and colors.
H_eqs_labels = c("Global", "Beam", "Diffuse")
H_eqs_cols = wes_palette("Darjeeling1", 3)

# Albedo.
al = 0.1    

# Net flux function type
#   1 for 1989 lookup table.
#   2 for 1990 lookup table.
#   3 for the analytical expresion.
nfft = 3

# Function parameters.
phi = 22.3
al = 0.1
tau = 0.5
Ls_seq = 1:360

ylim = c(1000, 4000)

# Plot type options:
#   1 - Line.
#   2 - Stacked bars.
#   3 - Besides bars.
plot_type = 1

dev.new()

# Empty data matrix that will contain calculate insolation values..
data_matrix = matrix(NA, nrow=3, ncol=length(Ls_seq))

H_index = 1
for(H_eq in H_eqs){
  for(Ls in Ls_seq){
    H_h = H_eq(Ls=Ls, phi=phi, tau=tau, al=al, nfft=nfft)
    # Populate data matrix.
    data_matrix[H_index, Ls] = H_h
  }
  
  H_index = H_index + 1
}

beside = if(plot_type == 2) FALSE else TRUE

# For stacked bars, only want to plot beam and diffuse. Not global.
data = if(isTRUE(beside)) data_matrix else data_matrix[-1,]

# Constrain label and color vectors accordingly
bar_labels = if(isTRUE(beside)) H_eqs_labels else H_eqs_labels[-1]
col = if(isTRUE(beside)) H_eqs_cols else H_eqs_cols[-1]

colnames(data) = Ls_seq
rownames(data) = bar_labels

if(plot_type == 1){
  plot(Ls_seq, data['Global',],
       xlab="Areocentric Longitude, Ls [deg]",
       ylab=paste("Insolation [Wh/m2-deg]", sep=""),
       ylim=ylim,
       type="l",
       col=H_eqs_cols[1],
       font.sub=2,
       cex.sub=1.2)
  
  lines(Ls_seq, data['Beam',], col=H_eqs_cols[2])
  lines(Ls_seq, data['Diffuse',], col=H_eqs_cols[3])
  
}else{
  barplot(data, col=col,
          beside=beside,
          xlab="Ls [deg]",
          ylab="Insolation [Wh/m2-deg]",
          xaxt='n',
          las=2,
          font.sub=2,
          cex.sub=1.2)
}

# Add a legend
if(plot_type == 1){
  legend("topright",
         H_eqs_labels,
         col = H_eqs_cols,
         cex=1, bty="n", lty=1)
}else{
  legend("topright",
         if(plot_type == 2) H_eqs_labels[-1] else H_eqs_labels,
         fill = if(plot_type == 2) H_eqs_cols[-1] else H_eqs_cols,
         cex=1, bty="n")
}   

title_template = "Variation of {{insolation}} insolation on Mars horizontal surface as a function of Areocentric Longitude\n(τ={{tau}}, ϕ={{phi}}°)"
title_data = list(insolation=if(plot_type == 2) "beam and diffuse" else "global, beam, and diffuse",
                  phi=phi,
                  tau=tau)

title = whisker.render(title_template, title_data)
mtext(title, side=3, line=-3, outer=TRUE)


