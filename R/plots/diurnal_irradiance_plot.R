# Function to plot variation of global, beam, and diffuse irradiance on Mars horizontal surface.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

library(wesanderson)

# Equation 17: Global irradiance on Mars horizontal surface (W/m2).
Gh_eq = dget(here("functions", "G_h.R"))

# Equation 18: Beam irradiance on Mars horizontal surface (W/m2).
Gbh_eq = dget(here("functions", "G_bh.R"))

# Determine an expression for the diffuse irradiance based on Eq. 17 and Eq. 18.
# Equation 16: The solar irradiance components on a horizontal Martian surface.
# Gh = Gbh + Gdh
Gdh_eq = dget(here("functions", "G_dh.R"))

# Daylight range.
daylight_range = dget(here("utils", "daylight_range.R"))

# Store all irradiance equations and their labels
G_eqs = c(Gh_eq, Gbh_eq, Gdh_eq)
G_eqs_labels = c("Global irradiance", "Beam irradiance", "Diffuse irradiance")
G_eqs_cols = wes_palette("Darjeeling1", 3)

function(nfft, Ls, phi, tau, al, T_step=1, T_min=0, T_max=24, sub="", xlim=c(0, 24), ylim=c(0, 700), points=TRUE, smooth=TRUE, cols=G_eqs_cols, lwd=1, pch=3){
  
  # Get solar time range when there is daylight
  Ts_range = daylight_range(Ls=Ls, phi=phi, T_step=T_step, T_min=T_min, T_max=T_max)
  
  # Flag
  new_plot_initialized = FALSE
  
  G_index = 1
  for(G_eq in G_eqs){
    G_seq = c()
    
    # Calculate the irradiance for each given parameters.
    # Store the results in a sequence that will be used in the plot() function.
    for(T_solar in Ts_range){
      G = G_eq(Ls=Ls, phi=phi, T_s=T_solar, tau=tau, al=al, nfft=nfft)  
      G_seq = c(G_seq, G)
    }
    
    # Plot
    if(length(Ts_range) == length(G_seq)){
      if(G_index == 1){
        plot(if(isTRUE(points)) Ts_range else NULL, 
             if(isTRUE(points)) G_seq else NULL, 
             xlab="Solar Time [h]", ylab="Irradiance [W/m2]",
             xlim=xlim, ylim=ylim,
             pch=pch,
             lwd=lwd,
             col=cols[G_index],
             sub=sub,
             font.sub=2,
             cex.sub=1.2)
        
        if(isTRUE(smooth)){
          smooth_line = smooth.spline(Ts_range, G_seq, spar=0.35)
          lines(smooth_line, col=cols[G_index], lwd=lwd)
          
        }else{
          lines(Ts_range, G_seq, col=cols[G_index], lwd=lwd)
        }
        
        new_plot_initialized = TRUE
      }else{
        # Make sure we are plotting on a new plot when needed.
        tryCatch({
          if(new_plot_initialized == TRUE){
            if(isTRUE(smooth)){
              smooth_line = smooth.spline(Ts_range, G_seq, spar=0.35)
              lines(smooth_line, col=cols[G_index], lwd=lwd)
              
            }else{
              lines(Ts_range, G_seq, col=cols[G_index], lwd=lwd)
            }
            
            points(if(isTRUE(points)) Ts_range else NULL, 
                   if(isTRUE(points)) G_seq else NULL, 
                   pch=pch,
                   col=cols[G_index])
          }else{
            stop("New plot has not been initialized.")
          }
        },
        warning = function(w) {
          # Do nothing
        },
        error = function(e) {
          # Enter here when following error occurs: plot.new has not been called yet
          plot(Ts_range, G_seq,
               xlab="Solar Time [h]", ylab="Irradiance [W/m2]",
               xlim=xlim, ylim=ylim,
               pch=pch,
               lwd=lwd,
               col=cols[G_index],
               sub=sub,
               font.sub=2,
               cex.sub=1.2)
          
          if(isTRUE(smooth)){
            smooth_line = smooth.spline(Ts_range, G_seq, spar=0.35)
            lines(smooth_line, col=cols[G_index], lwd=lwd)
            
          }else{
            lines(Ts_range, G_seq, col=cols[G_index], lwd=lwd)
          }
          
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
      message(paste("Could not calculate ", G_eqs_labels[G_index] , " for latitude ϕ=", phi, "°.", sep=""))
    }
    
    if(G_index == 3){
      G_index = 1
    }else{
      G_index = G_index + 1
    }
  }
} 