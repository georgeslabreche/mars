library(configr)
library(here)
library(wesanderson)
library(whisker)

config_filepath = here('main', 'config.yml')
config = read.config(file=config_filepath)

diurnal_insolation_plot = dget(here("functions/plots", "diurnal_insolation_plot.R"))

Ls = 71 # Ls for Aphelion
phis = c(-40, -20, 0, 40) # Planetary latitudes
tau = 0.5 # Optical depth tau factor.
al = 0.1  # Albedo. 

# Net flux function type 3 for the analytical expression.
nfft = 3

# Legend labels and colors.
I_eqs_labels = c("Global", "Beam", "Diffuse")
I_eqs_cols = wes_palette("Darjeeling1", 3)

phi_index = 1

for(phi in phis){
  dev.new()
  par(bg='white')
  
  diurnal_insolation_plot(nfft=nfft, Ls=Ls, phi=phi, tau=tau, al=al, T_step=1,
                          sub="", xlim=c(1, 24), ylim=c(0, 500),
                          points=FALSE, smooth=TRUE, plot_type=3)

  # Add a legend to first plot.
  if(phi_index == 1){
    legend("topright",
          I_eqs_labels,
          col = I_eqs_cols,
          cex=1, lwd=3)
  }
  
  
  title_template = "Diurnal insolation variation {{index}} for Ls {{Ls}}, phi {{phi}}, and tau {{tau}}"
  title_data = list(index=phi_index, Ls=Ls, phi=phi, tau=tau)
  title = whisker.render(title_template, title_data)
  
  plot_filepath = paste(config$output$marsenv, slugify(title, space_replace="-"), ".png", sep="")
  dev.copy(png, plot_filepath)
  dev.off()
  
  phi_index = phi_index + 1
  
}

