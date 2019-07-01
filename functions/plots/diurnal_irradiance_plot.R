# Function to plot variation of global, beam, and diffuse irradiance on Mars horizontal surface.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

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

# Mars obliquity of rotation axis [W/m2].
delta_0 = 24.936

function(nfft, Ls, phi, tau, al, Ts=NULL, T_step=1, sub="", xlim=NULL, ylim=c(0,550), points=TRUE, smooth=TRUE){

  # Equation 7 (1990): Declination angle [rad].
  delta = asin(sin(delta_0*pi/180) * sin(Ls*pi/180))
  
  # Equations 16 (Update 1991): Figure out if it is polar night or polar day.
  polar_flag = -tan(delta) * tan(phi*pi/180)
  
  # For polar nights and polar days.
  #   Polar night (polar_flag < -1), no solar irradiance.
  #   Polar day (polar_flag > 1), constant solar irradiance.  
  if(polar_flag < -1 || polar_flag > 1){
    # If no range is given, just cover the entire solar time range.
    Ts = if(!is.null(Ts)) Ts else 0:24
  }
  
  # For non polar nights or non polar days.
  else{
    
    # Equation 8 (1993): Sunrise hour angle [deg].
    omega_sr = -acos(-tan(phi*pi/180) * tan(delta)) * 180/pi
    
    # Equation 9 (1990): Sunset hour angle [deg].
    # Due to symmetry, it's just the sign opposite of the sunrise angle.
    omega_ss = acos(-tan(phi*pi/180) * tan(delta)) * 180/pi
    
    # Equation 8 (1990): Hour angle.
    # From Appelbaum, Joseph & Flood, Dennis. (1990):
    #   The ratio of Mars to Earth length of day is 24.65/24.
    #   It is convenient, for calculation purposes, to define a Mar hour
    #   by dividing the Martian day into 24 hr. Using the same relationship
    #   between the Mars solar time T and the hour angle as for the Earth.
    T_sr = (omega_sr + 180) / 15
    T_ss = (omega_ss + 180) / 15
    
    if(!is.null(Ts)){
      
      # Start time is before sunrise time
      if(Ts[1] < T_sr){
        # Constrain the given solar time range to times after the sunrise time.
        Ts = Ts[Ts >= T_sr]
      }
      
      # End time is after sunset time
      if(Ts[length(Ts)] > T_ss){
        # Constrain the given solar time range to time before to the sunset time.
        Ts = Ts[Ts <= T_ss]
      }
    }else{
      # If solar time range has been given, build a solar time range based on sunrise and sunset times.
      Ts = seq(T_sr, T_ss, T_step)
      if(Ts[length(Ts)] != T_ss){
        Ts = c(Ts, T_ss)
      }
    }
  }
  
  
  new_plot_initialized = FALSE
  
  G_index = 1
  for(G_eq in G_eqs){
    G_seq = c()
    
    # Calculate the irradiance for each given parameters.
    # Store the results in a sequence that will be used in the plot() function.
    for(T_solar in Ts){
      Z = Z_eq(Ls, T_solar, phi, nfft)
      
      # FIXME: Update G_eq so that they figure out if its a polar night or day on their own.
      G = if(polar_flag > 1) 0 else G_eq(Ls, Z, tau, al, nfft)  
      G_seq = c(G_seq, G)
    }
    
    # Plot
    if(length(Ts) == length(G_seq)){
      if(G_index == 1){
        plot(if(isTRUE(points)) Ts else NULL, 
             if(isTRUE(points)) G_seq else NULL, 
             xlab="Solar Time [h]", ylab="Irradiance [W/m2]",
             xlim=xlim, ylim=ylim,
             pch=3,
             col=G_eqs_cols[G_index],
             sub=sub,
             font.sub=2,
             cex.sub=1.2)
        
        if(isTRUE(smooth)){
          smooth_line = smooth.spline(Ts, G_seq, spar=0.35)
          lines(smooth_line, col=G_eqs_cols[G_index])
          
        }else{
          lines(Ts, G_seq, col=G_eqs_cols[G_index])
        }
        
        new_plot_initialized = TRUE
      }else{
        # Make sure we are plotting on a new plot when needed.
        tryCatch({
          if(new_plot_initialized == TRUE){
            if(isTRUE(smooth)){
              smooth_line = smooth.spline(Ts, G_seq, spar=0.35)
              lines(smooth_line, col=G_eqs_cols[G_index])
              
            }else{
              lines(Ts, G_seq, col=G_eqs_cols[G_index])
            }
            
            points(if(isTRUE(points)) Ts else NULL, 
                   if(isTRUE(points)) G_seq else NULL, 
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
          plot(Ts, G_seq,
               xlab="Solar Time [h]", ylab="Irradiance [W/m2]",
               xlim=xlim, ylim=ylim,
               pch=3,
               col=G_eqs_cols[G_index],
               sub=sub,
               font.sub=2,
               cex.sub=1.2)
          
          if(isTRUE(smooth)){
            smooth_line = smooth.spline(Ts, G_seq, spar=0.35)
            lines(smooth_line, col=G_eqs_cols[G_index])
            
          }else{
            lines(Ts, G_seq, col=G_eqs_cols[G_index])
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