# Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

# Load libraries
library(here)
library(wesanderson)

# Plot function.
diurnal_irradiance_plot = dget(here("functions/plots", "diurnal_irradiance_plot.R"))

# Legend labels and colors.
G_eqs_labels = c("Global irradiance", "Beam irradiance", "Diffuse irradiance")
G_eqs_cols = wes_palette("Darjeeling1", 3)

# Albedo.
al = 0.1    

# Net flux function type
#   1 for 1989 lookup table.
#   2 for 1990 lookup table.
#   3 for the analytical expresion.
nfft = 1  

# Tau list options
taus_clear_day = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
taus_selected = c(0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0)

# Areocentric Longitude values (deg).
Ls_VE = 0       # Vernal Equinox - Dust Storm Season ends.
Ls_A = 71       # APHELION.
Ls_SS = 90      # Summer Solstice.
Ls_AE = 180     # Autumn Equinox - Dust Sand storm Season begins.
Ls_P = 248      # PERIPHELION - Dust Storm Season.
Ls_WS = 270     # Winter Solstice - Dust Storm Season.
Ls_seq = c(Ls_VE, Ls_A, Ls_SS, Ls_AE, Ls_P, Ls_WS)
Ls_lbl_seq = c('Vernal Equinox', 'Aphelion', 'Summer Solstice', 'Autumn Equinox', 'Periphelion', 'Winter Solstice')

tau = 0.5
phi = 22.3 # Latitude of at Viking Lander VL1 [deg]. a

# Default plotting parameters.
Ts=0:24
T_step=1
xlim=NULL
include_points = TRUE
smooth_lines = TRUE

# Calculate irradiances.

##############################################################################################################################
# Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different areocentric longitudes. #
##############################################################################################################################
dev.new()
par(mfrow=c(2,4))
Ls_index = 1
for(Ls in Ls_seq){
  sub = paste(Ls_lbl_seq [Ls_index], " (Ls = ", Ls, "°)", sep="")
  diurnal_irradiance_plot(nfft=nfft, Ls=Ls, phi=phi, tau=tau, al=al, Ts=Ts, T_step=T_step, sub=sub, xlim=xlim, points=include_points, smooth=smooth_lines)
  Ls_index = Ls_index + 1
}
mtext(paste("Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different areocentric longitudes\n", paste("(τ=", tau, ", ϕ=", phi, "°)", sep="")), side = 3, line = -3, outer = TRUE)

# Add a legend
plot.new()
legend("topleft",
       G_eqs_labels,
       col = G_eqs_cols,
       cex=1, bty="n", lty=1)

######################################################################################################################
# Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different optical depths. #
######################################################################################################################

# Select an areocentric longitude.
Ls = Ls_A

# Select optical depth tau factors.
taus = taus_selected

dev.new()
par(mfrow=c(3,4))
for(tau in taus){
  sub = paste("τ = ", tau, sep="")
  diurnal_irradiance_plot(nfft=nfft, Ls=Ls, phi=phi, tau=tau, al=al, Ts=Ts, T_step=T_step, sub=sub, xlim=xlim, points=include_points, smooth=smooth_lines)
}
mtext(paste("Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different optical depths\n", paste("(Ls=", Ls, "°, ϕ=", phi, "°)", sep="")), side = 3, line = -3, outer = TRUE)

# Add a legend
plot.new()
legend("topleft",
       G_eqs_labels,
       col = G_eqs_cols,
       cex=1, bty="n", lty=1)

################################################################################################################
# Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different latitude. #
################################################################################################################

# Select an areocentric longitude.
Ls = Ls_A

# Select an optical depth tau factor.
tau = 0.5

# Select planetary latitudes
phis = seq(-50, 50, 10)

dev.new()
par(mfrow=c(3,4))
for(phi in phis){
  sub = paste("ϕ = ", phi, sep="")
  diurnal_irradiance_plot(nfft=nfft, Ls=Ls, phi=phi, tau=tau, al=al, Ts=Ts, T_step=T_step, sub=sub, xlim=xlim, points=include_points, smooth=smooth_lines)
}
mtext(paste("Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different latitudes\n", paste("(Ls=", Ls, "°, τ=", tau, "°)", sep="")), side = 3, line = -3, outer = TRUE)

# Add a legend
plot.new()
legend("topleft",
       G_eqs_labels,
       col = G_eqs_cols,
       cex=1, bty="n", lty=1)