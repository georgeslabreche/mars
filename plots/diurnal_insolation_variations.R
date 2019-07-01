# Diurnal variation of global, beam, and diffuse insolation on Mars horizontal surface.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

# Load libraries
library(here)
library(wesanderson)
library(whisker)

# Plot function.
diurnal_insolation_plot = dget(here("functions/plots", "diurnal_insolation_plot.R"))

# Legend labels and colors.
I_eqs_labels = c("Global", "Beam", "Diffuse")
I_eqs_cols = wes_palette("Darjeeling1", 3)

# Albedo.
al = 0.1    

# Net flux function type
#   1 for 1989 lookup table.
#   2 for 1990 lookup table.
#   3 for the analytical expresion.
nfft = 3

# Tau list options
taus_clear_day = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
taus_full_range = c(0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0)
taus_limited_range = c(0.1, 1.0, 3.0, 6.0)

Ls_list = list(
  "Vernal Equinox" = 0,
  "Aphelion" = 71,
  "Summer Solstice" = 90,
  "Autumn Equinox" = 180,
  "Periphelion" = 248,
  "Winter Solstice" = 270)

# Default plotting parameters.
tau = 0.5
phi = 22.3 # Latitude of at Viking Lander VL1 [deg].

T_step = 1
ylim = c(0, 2000)
xlim = c(T_step, 24)
include_points = TRUE
smooth_lines = TRUE

plot_type = 3

######################################################################################################################
# Diurnal variation of global, beam, and diffuse insolation on Mars horizontal surface for different optical depths. #
######################################################################################################################

# Select an areocentric longitude.
Ls = Ls_list$Aphelion

# Select optical depth tau factors.
taus = taus_limited_range
taus = c(0.5)

dev.new()
#par(mfrow=c(3,4))
#par(mfrow=c(2,2))
par(mfrow=c(1,1)) # For a single tau.

tau_index = 1
for(tau in taus){
  sub = paste("τ = ", tau, sep="")
  diurnal_insolation_plot(nfft=nfft, Ls=Ls, phi=phi, tau=tau, al=al, T_step=T_step, sub=sub, xlim=xlim, ylim=ylim, points=include_points, smooth=smooth_lines, plot_type=plot_type)
  
  # Add a legend
  if(tau_index == 1){
    if(plot_type == 1){
      legend("topleft",
             I_eqs_labels,
             col = I_eqs_cols,
             cex=1, bty="n", lty=1)
    }else{
      legend("topleft",
             if(plot_type == 2) I_eqs_labels[-1] else I_eqs_labels,
             fill = if(plot_type == 2) I_eqs_cols[-1] else I_eqs_cols,
             cex=1, bty="n")
    }   
  }
  
  tau_index = tau_index + 1
}

# Build and place the plot title.
title_template = "Diurnal variation of global, beam, and diffuse insolation on Mars horizontal surface for different optical depths\n(Ls={{Ls}}°, ϕ={{phi}}°)"

title_data = list(Ls=Ls, phi=phi)
title = whisker.render(title_template, title_data)

mtext(title, side=3, line=-3, outer=TRUE)

