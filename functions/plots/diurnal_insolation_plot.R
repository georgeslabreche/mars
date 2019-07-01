# Function to plot variation of global, beam, and diffuse irradiance on Mars horizontal surface.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

# Global hourly insolation on Mars horizontal surface [Wh/m2].
Ih_eq = dget(here("functions", "I_h.R"))

# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface [Wh/m2].
Ibh_eq = dget(here("functions", "I_bh.R"))

# Diffuse hourly insolation on Mars horizontal surface [Wh/m2].
Idh_eq = dget(here("functions", "I_dh.R"))

# Store all insolation equations and their labels
I_eqs = c(Ih_eq, Ibh_eq, Idh_eq)
I_eqs_labels = c("Global insolation", "Beam insolation", "Diffuse insolation")
I_eqs_cols = wes_palette("Darjeeling1", 3)

diurnal_line = function(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth, new_plot){
  # Label the unit correctly based on the desired insolation time range.
  ylab_h = if(T_step==1) "" else T_step
  
  # Plot
  if(isTRUE(new_plot)){
    plot(if(isTRUE(points)) Ts+T_step else NULL,
         if(isTRUE(points)) I_seq else NULL,
         xlab="Solar Time Range [h]", ylab=paste("Insolation [Wh/m2", "-", ylab_h, "h]", sep=""),
         xlim=xlim, ylim=ylim,
         pch=3,
         col=I_eqs_cols[I_index],
         sub=sub,
         font.sub=2,
         cex.sub=1.2)
  }
  
  if(isTRUE(smooth)){
    smooth_line = smooth.spline(Ts+T_step, I_seq, spar=0.35)
    lines(smooth_line, col=I_eqs_cols[I_index])
    
  }else{
    lines(Ts+T_step, I_seq, col=I_eqs_cols[I_index])
  }
  
  if(isFALSE(new_plot)){
    points(if(isTRUE(points)) Ts+T_step else NULL, 
           if(isTRUE(points)) I_seq else NULL, 
           pch=3,
           col=I_eqs_cols[I_index])
  }
}

plot_diurnal_line = function(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth){
  if(length(Ts) == length(I_seq)){
    if(I_index == 1){
      
      # New Plot
      diurnal_line(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth, TRUE)
      
    }else{
      # Make sure we are plotting on a new plot when needed.
      tryCatch({
        diurnal_line(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth, FALSE)
      },
      warning = function(w) {
        # Do nothing
      },
      error = function(e) {
        # Enter here when following error occurs: plot.new has not been called yet.
        # Plot
        diurnal_line(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth, TRUE)
      },
      finally = {
        # Do nothing
      })
    }
      
    
  }else{
    # Using the f_89 or f_90 lookup table based implementations of the net flux function may
    # result feeding that function a rounded Z parameter that does not exist in the lookup tables.
    # This will result in an error which we are handling here by not plotting the affected irradiance type.
    message(paste("Could not calculate ", I_eqs_labels[I_index] , " for latitude ϕ=", phi, "°.", sep=""))
  }
}

plot_diurnal_stacked_bars = function(data_matrix, T_step, sub, xlim, ylim, x_labels, beside){
  
  # For stacked bars, only want to plot beam and diffuse. Not global.
  data = if(isTRUE(beside)) data_matrix else data_matrix[-1,]
  
  # Constrain label and color vectors accordingly
  #bar_labels = if(isTRUE(beside)) I_eqs_labels else I_eqs_labels[-1,]
  col = if(isTRUE(beside)) I_eqs_cols else I_eqs_cols[-1]
  
  colnames(data) = x_labels
  #rownames(data) = bar_labels
  
  # There is no need to plot zero insolation.
  # Filter out data points where global insolation is 0.
  if(isTRUE(beside)){
    data = data[, data[1,] > 0]
  }else{
    data = data[, data[1,] + data[2,] > 0]
  }
  
  # Plot
  # Label the unit correctly based on the desired insolation time range.
  ylab_h = if(T_step==1) "" else T_step
  
  barplot(data, col=col,
          beside=beside,
          xlab="Solar Time Range [h]",
          ylab=paste("Insolation [Wh/m2", "-", ylab_h, "h]", sep=""),
          sub=sub,
          font.sub=2,
          cex.sub=1.2)
}

plot_diurnal_bubble_grid = function(){
  #http://chartartistry.blogspot.com/2016/02/dot-charts-alternative-to-stacked.html
}

function(nfft, Ls, phi, tau, al, T_step=1, sub="", xlim=c(0, 24), ylim=c(0,550), points=TRUE, smooth=TRUE, plot_type){
  # Empty data matrix that will contain calculate insolation values..
  data_matrix = matrix(NA, nrow = 3, ncol = 24/T_step)
  
  I_index = 1
  for(I_eq in I_eqs){

    # Calculate the insolation for each given parameters.
    # Store the results in a sequence that will be used in the plot() function.
    Ts = seq(0, 24-T_step, T_step)
    x_labels = c()
    T_range_index = 1
    
    for(T_start in Ts){
      T_end = T_start + T_step
      
      x_labels = c(x_labels, paste(T_start, "-", T_end, sep=""))
      I = I_eq(Ls, phi, tau, T_start, T_end, al, nfft)  

      # Populate data matrix.
      data_matrix[I_index, T_range_index] = I
      
      T_range_index = T_range_index + 1
    }
    
    # Plot
    if(plot_type == 1){
      I_seq = data_matrix[I_index,]
      plot_diurnal_line(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth)
    }
    
    if(I_index == 3){
      I_index = 1
    }else{
      I_index = I_index + 1
    }
  }
  
  if(plot_type == 2){
    plot_diurnal_stacked_bars(data_matrix, T_step, sub, xlim, ylim, x_labels, FALSE)
    
  }else if(plot_type == 3){
    plot_diurnal_stacked_bars(data_matrix, T_step, sub, xlim, ylim, x_labels, TRUE)
  }
} 