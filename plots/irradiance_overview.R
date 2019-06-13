# Generate various plots describing solar radiation on Mars.
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# Load
library(wesanderson)

# Equation 17: Global irradiance on Mars horizontal surface (W/m2).
Gh_eq = dget(here("functions", "G_h.R"))

# Equation 18: Beam irradiance on Mars horizontal surface (W/m2).
Gbh_eq = dget(here("functions", "G_bh.R"))

# Determine an expression for the diffuse irradiance based on Eq. 17 and Eq. 18.
# Equation 16: The solar irradiance components on a horizontal Martian surface.
# Gh = Gbh + Gdh
Gdh_eq = dget(here("functions", "G_dh.R"))
  
# Store all irradiance equations and their labels
G_eqs = c(Gh_eq, Gbh_eq, Gdh_eq)
G_eqs_labels = c("global irradiance", "beam irradiance", "diffuse irradiance")

al = 0.1    # Albedo.
nfft = 1    # Net flux function type (1 for f_89, 2 for f_90, and 3 for f).

# Optical depth and zenith angle values.
# FIXME: Edit this to grab based on nfft.
f_all_taus = dget(here("functions", "f_all_taus.R"))
f_all_Zs = dget(here("functions", "f_all_Zs.R"))

# Areocentric Longitude values (deg).
Ls_VE = 0       # Vernal Equinox - Dust Storm Season ends.
Ls_A = 71       # APHELION.
Ls_SS = 90      # Summer Solstice.
Ls_AE = 180     # Autumn Equinox - Dust Storm Season begins.
Ls_P = 248      # PERIPHELION - Dust Storm Season.
Ls_WS = 270     # Winter Solstice - Dust Storm Season.
Ls_seq = c(Ls_VE, Ls_A, Ls_SS, Ls_AE,  Ls_P, Ls_WS)
Ls_lbl_seq = c('Vernal Equinox', 'Aphelion', 'Summer Solstice', 'Autumn Equinox', 'Periphelion', 'Winter Solstice')

# Tau list options
taus_nfft = f_all_taus(nfft)
taus_clear_day = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
taus_selected = c(0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0)

# Which taus vector to work with.
taus = taus_selected

# Zenith angle options
zenith_angles = f_all_Zs(nfft)

#####################################################################
# Equation 17: Global irradiance on Mars horizontal surface (W/m2). #
# Equation 18: Beam irradiance on Mars horizontal surface (W/m2).   #
# Equation 16: Diffuse irradiance on Mars horizontal surface(W/m2). #
#####################################################################

###
# Title: Variation of global, bream, and diffuse irradiance on Mars horizontal for different optical depths.
# Subtitle: Effect of areocentric longitude with sun zenith angle as a parameter.
###
eq_index = 1
for(G_eq in G_eqs){
  dev.new()
  par(mfrow=c(4,3))
  for(tau in taus){
    color_index = 1
  
    for(Z in zenith_angles){
      curve(G_eq(x, Z, tau, al, nfft), 0, 360, 360,
            add=ifelse(color_index>1, TRUE, FALSE),
            ylim=c(0, 700),
            xlab="Ls [deg]", ylab="Gh [W/m2]",
            sub=paste("τ = ", tau, sep=""),
            font.sub=2,
            cex.sub=1.2,
            type="l",
            col=wes_palette("FantasticFox1", length(zenith_angles), type = "continuous")[color_index])
  
      color_index = color_index + 1
    }
  }
  
  mtext(paste("Variation of", G_eqs_labels[eq_index], "on Mars horizontal surface for different optical depths\nEffect of areocentric longitude with sun zenith angle as a parameter"), side = 3, line = -3, outer = TRUE)
  
  # Add a legend
  plot.new()
  legend("top", title="Zenith Angle",
         paste("Z = ", zenith_angles, "°", sep=""),
         col = wes_palette("FantasticFox1", length(zenith_angles), type = "continuous"),
         cex=0.8, bty="n", lty=1)
  eq_index = eq_index + 1
}

###
# Title: Variation of global, bream, and diffuse irradiance on Mars horizontal for different sun zenith angles.
# Subtitle: Effect of areocentric longitude with optical depth as a parameter.
###
eq_index = 1
for(G_eq in G_eqs){
  dev.new()
  par(mfrow=c(4,3))
  for(Z in rev(zenith_angles)){
    color_index = 1

    for(tau in taus){
      curve(G_eq(x, Z, tau, al, nfft), 0, 360, 360,
            add=ifelse(color_index>1, TRUE, FALSE),
            ylim=c(0, 700),
            xlab="Ls [deg]", ylab="Gh [W/m2]",
            sub=paste("Z = ", Z, "°", sep=""),
            font.sub=2,
            cex.sub=1.2,
            type="l",
            col=wes_palette("FantasticFox1", length(taus), type = "continuous")[color_index])
      color_index = color_index + 1
    }
  }

  plot.new()
  legend("topleft", title="Optical Depth",
         paste("τ = ", taus, sep=""),
         col=wes_palette("FantasticFox1", length(taus), type = "continuous"),
         cex=0.8, bty="n", lty=1)

  mtext(paste("Variation of", G_eqs_labels[eq_index], "on Mars horizontal surface for different solar zenith angles\nEffect of areocentric longitude with optical depth as a parameter"), side = 3, line = -3, outer = TRUE)
  eq_index = eq_index + 1
}

###
# Title: Variation of global, bream, and diffuse irradiance on Mars horizontal surface for different areocentric longitudes.
# Subtitle: Effect of sun zenith angle with optical depth as a parameter.
###
eq_index = 1
for(G_eq in G_eqs){
  dev.new()
  par(mfrow=c(3,3))
  for(Ls in Ls_seq){
    index = 1
    for(tau in taus){
      if(index == 1){
        plot(zenith_angles, G_eq(Ls, zenith_angles, tau, al, nfft),
             ylim=c(0, 700),
             xlab="Z [deg]", ylab="Gh [W/m2]",
             sub=paste("Ls = ", Ls, "°", sep=""),
             font.sub=2,
             cex.sub=1.2,
             type="l",
             col=wes_palette("FantasticFox1", length(taus), type = "continuous")[index])
      }else{
        lines(zenith_angles, G_eq(Ls, zenith_angles, tau, al, nfft),
              sub=paste("Ls = ", Ls, "°", sep=""),
              font.sub=2,
              cex.sub=1.2,
              type="l",
              col=wes_palette("FantasticFox1", length(taus), type = "continuous")[index])
      }
      index = index + 1
    }
  }

  plot.new()
  legend("top", title="Optical Depth",
         paste("τ = ", taus, sep=""),
         col=wes_palette("FantasticFox1", length(taus), type = "continuous"),
         cex=0.8, bty="n", lty=1)
  mtext(paste("Variation of", G_eqs_labels[eq_index], "on Mars horizontal surface for different areocentric longitudes\nEffect of sun zenith angle with optical depth as a parameter"), side = 3, line = -3, outer = TRUE)
  eq_index = eq_index + 1
}

###
# Title: Variation of global, bream, and diffuse irradiance on Mars horizontal surface for different areocentric latitudes.
# Subtitle: Effect of optical depth with sun zenith angle as a parameter.
###
eq_index = 1
for(G_eq in G_eqs){
  dev.new()
  par(mfrow=c(3,3))
  Ls = 180
  for(Ls in Ls_seq){
    index = 1
    for(zenith_angle in zenith_angles){
      if(index == 1){
        plot(taus, G_eq(Ls, zenith_angle, taus, al, nfft),
             ylim=c(0, 700),
             xlab="τ", ylab="Gh [W/m2]",
             sub=paste("Ls = ", Ls, "°", sep=""),
             font.sub=2,
             cex.sub=1.2,
             type="l",
             col=wes_palette("FantasticFox1", length(zenith_angles), type = "continuous")[index])
      }else{
        lines(taus, G_eq(Ls, zenith_angle, taus, al, nfft),
              sub=paste("Ls = ", Ls, "°", sep=""),
              font.sub=2,
              cex.sub=1.2,
              type="l",
              col=wes_palette("FantasticFox1", length(zenith_angles), type = "continuous")[index])
      }
      index = index + 1
    }
  }

  plot.new()
  legend("top", title="Zenith Angle",
         paste("Z = ", zenith_angles, "°" , sep=""),
         col=wes_palette("FantasticFox1", length(zenith_angles), type = "continuous"),
         cex=0.8, bty="n", lty=1)
  mtext(paste("Variation of", G_eqs_labels[eq_index], "on Mars horizontal surface for different areocentric longitudes\nEffect of optical depth with sun zenith angle as a parameter"), side = 3, line = -3, outer = TRUE)

  eq_index = eq_index + 1
}


