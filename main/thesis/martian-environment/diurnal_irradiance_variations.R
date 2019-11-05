# Load libraries
library(configr)
library(here)
library(wesanderson)

config_filepath = here('main', 'config.yml')
config = read.config(file=config_filepath)

# Plot function.
diurnal_irradiance_plot = dget(here("functions/plots", "diurnal_irradiance_plot.R"))

Ls = 248 # Ls for Periphelion.
phis = c(-20, 0, 20, 40) # Planetary latitudes.
tau = 0.5 # Optical depth tau factor.
al = 0.1  # Albedo. 

# Net flux function type 3 for the analytical expression.
nfft = 3

# Legend labels and colors.
G_eqs_labels = c("Global", "Beam", "Diffuse")
G_eqs_cols = wes_palette("Darjeeling1", 3)


phi_index = 1
for(phi in phis){
  dev.new()
  par(bg='white')
  
  # FIXME: Not working with smooth=TRUE
  diurnal_irradiance_plot(nfft=nfft, Ls=Ls, phi=phi, tau=tau, al=al, points=FALSE, smooth=FALSE, cols=G_eqs_cols, lwd=3)
  
  # Add a legend to first plot.
  if(phi_index == 1){
    legend("topright",
           G_eqs_labels,
           col = G_eqs_cols,
           cex=1, lwd=3)
  }
  
  title_template = "Diurnal irradiance variation {{index}} for Ls {{Ls}}, phi {{phi}}, and tau {{tau}}"
  title_data = list(index=phi_index, Ls=Ls, phi=phi, tau=tau)
  title = whisker.render(title_template, title_data)
  
  plot_filepath = paste(config$output$marsenv, slugify(title, space_replace="-"), ".png", sep="")
  dev.copy(png, plot_filepath)
  dev.off()
  
  phi_index = phi_index + 1

}
