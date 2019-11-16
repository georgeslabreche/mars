Sys.setenv(NET_FLUX_FUNCTION_TYPE = "polynomial")

# The function.
#
#' Title
#'
#' @return
#' @export
f_lookup_taus = function(){
  
  net_flux_function_type = Sys.getenv("NET_FLUX_FUNCTION_TYPE")
  
  if(net_flux_function_type == "lookup_v1"){
    as.numeric(rownames(df_netflux_0p1_1990))
    
  }else if(net_flux_function_type == "lookup_v2"){
    as.numeric(rownames(df_netflux_0p1_1991))
    
  }else{
    stop("This function is only available when the NET_FLUX_FUNCTION_TYPE environment variable is set to 'lookup_v1' or 'lookup_v2'.")
  }
}

# The function.
#' Title
#' 
#' @return
#' @export
f_lookup_Zs = function(){
  
  net_flux_function_type = Sys.getenv("NET_FLUX_FUNCTION_TYPE")
  
  if(net_flux_function_type == "lookup_v1"){
    as.numeric(gsub("X", "", colnames(df_netflux_0p1_1990)))
    
  }else if(net_flux_function_type == "lookup_v2"){
    as.numeric(gsub("X", "", colnames(df_netflux_0p1_1991)))
    
  }else{
    stop("This function is only available when the NET_FLUX_FUNCTION_TYPE environment variable is set to 'lookup_v1' or 'lookup_v2'.")
  }
}

#' The coefficient lookup function.
#'
#' Source: Table IV - Normalized Net Flux Function Coefficients in Appelbaum, Joseph & Flood, Dennis (1990) Update 1990.
#' 
#' @param i 
#' @param j 
#' @param k 
#'
#' @return
p = function(i, j, k){
  
  if(k==0){
    coefficient = df_netflux_k0_1990[j+1, paste("X", i, sep="")]
  }else if(k==1){
    coefficient = df_netflux_k1_1990[j+1, paste("X", i, sep="")]
  }else{
    stop("Unsupported k value.")
  }
  
  return(coefficient)
}

#' The normalized net flux function's lookup table lookup.
#'
#' The net solar flux integrated over the solar spectrum on the Martian
#' surface was calculated by Pollack based on multiple wavelength and
#' multiple scattering of the solar radiation.
#' 
#' Source: Table III in Appelbaum, Joseph & Flood, Dennis. (1990). 
#'
#' @param z Zenith angle [deg].
#' @param tau Optical depth.
#' @param al Albedo, can only be 0.1.
#'
#' @return Normalized net flux.
f_lookup_v1 = function(z, tau, al=0.1){
  
  if(al != 0.1){
    stop("The albedo can only be 0.1 when using f_lookup_v1.")
  }
  
  return(
    unlist( # Unlist in case a sequence of Zs are given instead of a single value (i.e. in the case of integrations).
      df_netflux_0p1_1990[df_netflux_0p1_1990$tau == toString(tau), paste("X", z, sep="")],
      use.names=FALSE
    )
  )
}

#' The normalized net flux function's lookup table lookup.
#' 
#' Source: Solar radiation on Mars: Update 1990.
#'  Table III (a) and (b) for an albedo of 0.1.
#'  Table IV  (a) and (b) for an albedo of 0.4.
#'  
#' @param z Zenith angle [deg].
#' @param tau Optical depth.
#' @param al Albedo, can be 0.1 or 0.4.
#'
#' @return Normalized net flux.
f_lookup_v2 = function(z, tau, al=0.1){
  
  nnff_df = NULL
  
  if(al == 0.1){
    nnff_df = df_netflux_0p1_1991
    
  }else if(al == 0.4){
    nnff_df = df_netflux_0p4_1991
    
  }else{
    stop("The albedo can only be 0.1 or 0.4 when using f_lookup_v2.")
  }
  
  return(
    unlist( # Unlist in case a sequence of Zs are given instead of a single value (i.e. in the case of integrations).
      nnff_df[nnff_df$tau == toString(tau), paste("X", z, sep="")],
      use.names=FALSE
    )
  )
}


#' The analytical expression of the normalized net flux function.
#' 
#' The mean error is about 0.7 percent for the full range. For zenith angles up to 40° the error is much smaller.
#' The largest error is for zenith angle of 80° and 85° and for tau greater than 5. The maximum error is about 7 percent.
#' At these large angles and opacities, the error has a minor effect on the calculated daily insolations.
#'
#' Source: Equation 20? in Appelbaum, Joseph & Flood, Dennis (1990) - Update 1990.
#' 
#' @param z Zenith angle [deg].
#' @param tau Optical depth.
#' @param al Albedo.
#'
#' @return Normalized net flux.
f_analytical = function(z, tau, al=0.1){
  
  # Check for and warn against parameters that would result in lagest errors (max. 7%).
  if(tau > 5){
    # TODO: Make warning.
    message(paste("Large error encountered with tau = ", tau, " greater than 5 (maximum error is 7%). ", 
                  "Consider using the f_89 and f_90 table lookup implementation of the normalized net flux function instead of its analytical expression.",
                  sep="")
    )
  }
  

  # Use ifelse in case this function is being invoked from an integration in which case Z can be a vector instead of a scalar.
  # If Z is a scalar and we use if() then the following issue will occur: "the condition has length > 1 and only the first element will be used."
  warning_msg = ifelse(z >= 80, paste("Possibly large error encountered with z = ", z, "° (maximum error is 7% for Z = 80° or Z = 85°). ",
                                      "Consider using the f_89 and f_90 table lookup implementation of the normalized net flux function instead of its analytical expression.\n",
                                      sep=""), "")
  
  # handle warning message.
  for(w_msg in warning_msg){
    if(w_msg != ""){
      # TODO: Make warning.
      message(w_msg)
    }
  }
  
  psum = 0
  for(i in seq(0,5,1)){
    for(j in seq(0,5,1)){
      for(k in seq(0,1,1)){
        psum = psum + p(i, j, k) * tau^i * (z/100)^j * al^k
      }
    }
  }
  
  return(psum * (1-al))
}



#' he analytical expression of the normalized net flux function.
#' 
#' Source: Equation 20 (1990).
#'
#' @param z Zenith angle [deg].
#' @param tau Optical depth.
#' @param al Albedo (ranges from 0.1 to 0.4).
#' @param pub_year
#'
#' @return Normalized net flux.
#' @export
f = function(z, tau, al=0.1){
  
  net_flux_function_type = Sys.getenv("NET_FLUX_FUNCTION_TYPE")
  
  if(net_flux_function_type == "polynomial"){
    net_flux = f_analytical(z, tau, al)
    
  }else if(net_flux_function_type == "lookup_v1"){
    net_flux = f_lookup_v1(z, tau, al)
    
  }else if(net_flux_function_type == "lookup_v2"){
    net_flux = f_lookup_v2(z, tau, al)
    
  }else{
    stop("The NET_FLUX_FUNCTION_TYPE environment variable should be set to 'lookup_v1', 'lookup_v2', or 'polynomial'.")
  }
  
  # Check if given Z results in NULL net flux.
  if(is.null(net_flux)){
    stop(paste("Sun zenith angle z = ", z ,"° is not available in the net flux look-up table. Consider using the analytical function instead.", sep=""))
    
  }
  
  # Check if given tau results in NA net flux.
  # Use ifelse in case this function is being invoked from an integration in which case net_flux can be a vector instead of a scalar.
  isNAs = ifelse(is.na(net_flux), TRUE, FALSE)
  for(isNA in isNAs){
    if(isTRUE(isNA)){
      stop(paste("Optical depth tau factor tau = ", tau," is not available in the net flux look-up table. Consider using the analytical function instead.", sep=""))
    }
  }
  
  return(net_flux) 
}