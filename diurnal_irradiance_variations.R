# Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface.
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# Equation 6: Zenith angle of the incident solar radiation (deg).
Z_eq = dget("functions/Z.R")

# Equation 17: Global irradiance on Mars horizontal surface (W/m2).
Gh_eq = dget("functions/G_h.R")

# Equation 18: Beam irradiance on Mars horizontal surface (W/m2).
Gbh_eq = dget("functions/G_bh.R")

# Determine an expression for the diffuse irradiance based on Eq. 17 and Eq. 18.
# Equation 16: The solar irradiance components on a horizontal Martian surface.
# Gh = Gbh + Gdh
Gdh_eq = dget("functions/G_dh.R")

# Store all irradiance equations and their labels
G_eqs = c(Gh_eq, Gbh_eq, Gdh_eq)
G_eqs_labels = c("global irradiance", "beam irradiance", "diffuse irradiance")
G_eqs_cols = c("red", "blue", "green")

# Tau list options
taus_clear_day = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
taus_selected = c(0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0)

# Areocentric Longitude values (deg).
Ls_VE = 0       # Vernal Equinox - Dust Storm Season ends.
Ls_A = 71       # APHELION.
Ls_SS = 90      # Summer Solstice.
Ls_AE = 180     # Autumn Equinox - Dust Sa = ctorm Season begins.
Ls_P = 248      # PERIPHELION - Dust Storm Season.
Ls_WS = 270     # Winter Solstice - Dust Storm Season.
Ls_seq = c(Ls_VE, Ls_A, Ls_SS, Ls_AE, Ls_P, Ls_WS)
Ls_lbl_seq = c('Vernal Equinox', 'Aphelion', 'Summer Solstice', 'Autumn Equinox', 'Periphelion', 'Winter Solstice')

# At Viking Lander VL1
#Ls = 153 # [deg]
tau = 0.5
phi = 22.3 # [deg N]
omega_l = 7 # [h]
omega_u = 17 # [h]

# Sequence of omega values to calculate irradiance.
omega_seq = seq(omega_l, omega_u, 1)

# Calculate irradiances.

##############################################################################################################################
# Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different areocentric longitudes. #
##############################################################################################################################
dev.new()
par(mfrow=c(3,2))
Ls_index = 1
for(Ls in Ls_seq){

  G_index = 1
  for(G_eq in G_eqs){
    G_seq = c()

    # Calculate the irradiance for each given parameters.
    # Store the results in a sequence that will be used in the plot() function.
    for(omega in omega_seq){
      Z = Z_eq(Ls, omega, phi)
      G = G_eq(Ls, Z, tau)
      G_seq = c(G_seq, G)
    }

    # Plot
    if(G_index == 1){
      plot(omega_seq, G_seq,
           xlab="Solar Time [h]", ylab="Gh [W/m2]",
           ylim=c(0,550),
           pch=3,
           col=G_eqs_cols[G_index],
           sub=paste(Ls_lbl_seq [Ls_index], " (Ls = ", Ls, "°)", sep=""),
           font.sub=2,
           cex.sub=1.2)

      smooth_line = smooth.spline(omega_seq, G_seq, spar=0.35)
      lines(smooth_line, col=G_eqs_cols[G_index])
    }else{
      smooth_line = smooth.spline(omega_seq, G_seq, spar=0.35)
      lines(smooth_line, col=G_eqs_cols[G_index])

      points(omega_seq, G_seq,
             pch=3,
             col=G_eqs_cols[G_index])
    }

    G_index = G_index + 1
  }

  Ls_index = Ls_index + 1
}
mtext(paste("Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different areocentric longitudes\n", paste("(τ=", tau, ", ϕ=", phi, "°)", sep="")), side = 3, line = -3, outer = TRUE)

######################################################################################################################
# Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different optical depths. #
######################################################################################################################

# Select an areocentric longitude.
Ls = Ls_A

# Select optical depth tau factors.
taus = taus_selected

dev.new()
par(mfrow=c(3,3))
for(tau in taus){

  G_index = 1
  for(G_eq in G_eqs){
    G_seq = c()

    # Calculate the irradiance for each given parameters.
    # Store the results in a sequence that will be used in the plot() function.
    for(omega in omega_seq){
      Z = Z_eq(Ls, omega, phi)
      G = G_eq(Ls, Z, tau)
      G_seq = c(G_seq, G)
    }

    # Plot
    if(G_index == 1){
      plot(omega_seq, G_seq,
           xlab="Solar Time [h]", ylab="Gh [W/m2]",
           ylim=c(0,550),
           pch=3,
           col=G_eqs_cols[G_index],
           sub=paste("τ = ", tau, sep=""),
           font.sub=2,
           cex.sub=1.2)

      smooth_line = smooth.spline(omega_seq, G_seq, spar=0.35)
      lines(smooth_line, col=G_eqs_cols[G_index])
    }else{
      smooth_line = smooth.spline(omega_seq, G_seq, spar=0.35)
      lines(smooth_line, col=G_eqs_cols[G_index])

      points(omega_seq, G_seq,
             pch=3,
             col=G_eqs_cols[G_index])
    }

    G_index = G_index + 1
  }
}
mtext(paste("Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different optical depths\n", paste("(Ls=", Ls, "°, ϕ=", phi, "°)", sep="")), side = 3, line = -3, outer = TRUE)


######################################################################################################################
# Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different optical depths. #
######################################################################################################################

# Select an areocentric longitude.
Ls = Ls_A

# Select an optical depth tau factor.
tau = 0.5

# Select latitudes
phis = seq(-40, 40, 10)

dev.new()
par(mfrow=c(3,3))
for(phi in phis){
  new_plot_initialized = FALSE
  
  G_index = 1
  for(G_eq in G_eqs){
    G_seq = c()
    
    # Calculate the irradiance for each given parameters.
    # Store the results in a sequence that will be used in the plot() function.
    for(omega in omega_seq){
      Z = Z_eq(Ls, omega, phi)
      G = G_eq(Ls, Z, tau)
      G_seq = c(G_seq, G)
    }
    
    # Plot
    if(length(omega_seq) == length(G_seq)){
      if(G_index == 1){
        plot(omega_seq, G_seq,
             xlab="Solar Time [h]", ylab="Gh [W/m2]",
             ylim=c(0, 600),
             pch=3,
             col=G_eqs_cols[G_index],
             sub=paste("ϕ = ", phi, "°", sep=""),
             font.sub=2,
             cex.sub=1.2)
        
        smooth_line = smooth.spline(omega_seq, G_seq, spar=0.35)
        lines(smooth_line, col=G_eqs_cols[G_index])
        
        new_plot_initialized = TRUE
      }else{
          
          tryCatch({
            if(new_plot_initialized == TRUE){
              smooth_line = smooth.spline(omega_seq, G_seq, spar=0.35)
              lines(smooth_line, col=G_eqs_cols[G_index])
            
              points(omega_seq, G_seq,
                   pch=3,
                   col=G_eqs_cols[G_index])
            }else{
               stop("New plot has not been initialized.")
             }
          },
          warning = function(w) {
            # Do nothing
          },
          error = function(e) {
            # Enter here when following error occurs: plot.new has not been called yet
            plot(omega_seq, G_seq,
                 xlab="Solar Time [h]", ylab="Gh [W/m2]",
                 ylim=c(0,550),
                 pch=3,
                 col=G_eqs_cols[G_index],
                 sub=paste("ϕ = ", phi, sep=""),
                 font.sub=2,
                 cex.sub=1.2)
            
            smooth_line = smooth.spline(omega_seq, G_seq, spar=0.35)
            lines(smooth_line, col=G_eqs_cols[G_index])
            
            new_plot_initialized = TRUE
          },
          finally = {
            # Do nothing
          })
        
      }
    }else{
      message(paste("Could not calculate ", G_eqs_labels[G_index] , " for latitude ϕ=", phi, "°", sep=""))
    }
    
    if(G_index == 3){
      G_index = 1
    }else{
      G_index = G_index + 1
    }
    
  }
}
mtext(paste("Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface for different latitudes\n", paste("(Ls=", Ls, "°, τ=", tau, "°)", sep="")), side = 3, line = -3, outer = TRUE)