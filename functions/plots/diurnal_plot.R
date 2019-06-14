# Function to plot variation of global, beam, and diffuse irradiance on Mars horizontal surface.
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# Equation 6: Zenith angle of the incident solar radiation (deg).
Z_eq = dget(here("functions", "Z.R"))

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
G_eqs_labels = c("Global irradiance", "Beam irradiance", "Diffuse irradiance")
G_eqs_cols = wes_palette("Darjeeling1", 3)

# Sequence of omega values to calculate irradiance.
omega_l = 7 # [h]
omega_u = 17 # [h]
omega_seq = seq(omega_l, omega_u, 1)

function(nfft, Ls, phi, tau, al, omegas=omega_seq, sub="", ylim=c(0,550)){
  new_plot_initialized = FALSE
  
  G_index = 1
  for(G_eq in G_eqs){
    G_seq = c()
    
    # Calculate the irradiance for each given parameters.
    # Store the results in a sequence that will be used in the plot() function.
    for(omega in omegas){
      Z = Z_eq(Ls, omega, phi, nfft)
      G = G_eq(Ls, Z, tau, al, nfft)
      G_seq = c(G_seq, G)
    }
    
    # Plot
    if(length(omegas) == length(G_seq)){
      if(G_index == 1){
        plot(omegas, G_seq,
             xlab="Solar Time [h]", ylab="Irradiance [W/m2]",
             ylim=ylim,
             pch=3,
             col=G_eqs_cols[G_index],
             sub=sub,
             font.sub=2,
             cex.sub=1.2)
        
        smooth_line = smooth.spline(omegas, G_seq, spar=0.35)
        lines(smooth_line, col=G_eqs_cols[G_index])
        
        new_plot_initialized = TRUE
      }else{
        # Make sure we are plotting on a new plot when needed.
        tryCatch({
          if(new_plot_initialized == TRUE){
            smooth_line = smooth.spline(omegas, G_seq, spar=0.35)
            lines(smooth_line, col=G_eqs_cols[G_index])
            
            points(omegas, G_seq,
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
          plot(omegas, G_seq,
               xlab="Solar Time [h]", ylab="Irradiance [W/m2]",
               ylim=ylim,
               pch=3,
               col=G_eqs_cols[G_index],
               sub=sub,
               font.sub=2,
               cex.sub=1.2)
          
          smooth_line = smooth.spline(omegas, G_seq, spar=0.35)
          lines(smooth_line, col=G_eqs_cols[G_index])
          
          new_plot_initialized = TRUE
        },
        finally = {
          # Do nothing
        })
        
      }
    }else{
      # Using the f_89 or f_90 lookup table based implementations of the net flux function may
      # result feeding that function a rounded Z parameter that does not exist in the lookup tables.
      # This will result in an error which we are handling here by not plotting the affected irradiance type.
      message(paste("Could not calculate ", G_eqs_labels[G_index] , " for latitude ϕ=", phi, "°.", " Try using the analytical expression of the net flux function instead of the lookup table.", sep=""))
    }
    
    if(G_index == 3){
      G_index = 1
    }else{
      G_index = G_index + 1
    }
    
  }
} 