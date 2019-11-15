# Equation 1 and 2: The optical depth tau factor function.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Landis, Geoffrey & Sherman, I. (1991). Solar radiation on Mars—Update 1991. Solar Energy. 50. 35-51. 10.1016/0038-092X(93)90006-A:
#   https://www.researchgate.net/publication/223850868_Solar_radiation_on_Mars-Update_1991
#
# From Appelbaum, Joseph & Landis, Geoffrey & Sherman, I (1991):
#     The optical depths at different latitudes other than the ones measured at the Viking Landers 
#   VL1 and VL2 are introduced in this article and are based on the assumption that the optical depth 
#   varies spatially and are expected to be greatest in the source region where dust is raised into the 
#   atmosphere. References [4,5] support this assumption. It is therefore assumed that the optical depths 
#   have a Gaussian distribution centered at the times of the global storms at VL1 and VL2. 
#   The relationship of the optical depth with latitudes may be expressed by a low order polynomial ratio. 
#   The optical depth function coefficients correspond to the measured opacities at VL1 and VL2. This 
#   function: τ(ϕ, Ls), was developed for the case of two global dust storms per Martian year; a model 
#   that corresponds to the Viking Lander observation. 
#     The location of the first global storm is well characterized, however there is some uncertainty 
#   about the location where the second 1977 storm started. We have therefore developed two models for 
#   global dust storm opacity, corresponding to latitude -10° and -30° locations for the second storm.
#
# The optical depth tau factor function.
#   phi     - Latitude [deg].
#   Ls      - Areocentric longitude [deg].
#   model   - Which model to apply:
#             1 - Assumes that the two global dust storms originated both at latitude ϕ = -30°
#                 and took place at Ls_1 = 215° (fist storm) and Ls_2 = 295° (second storm).
#             2 - Assumes that the first global dust storms originated at latitude ϕ_1 = -30°
#                 and the second one at latitude ϕ_2 = -10°.
#                 The times of the storm aaare the same as model 1, at Ls_1 = 215° (fist storm)
#                 and Ls_2 = 295° (second storm).
#              
function(phi, Ls, model=1){
  if (model == 1){
    
    a = 16787 * ((1 + phi/150) / (1917 + (phi+38.27)^2))
    b = 0.779 * exp(-(Ls-215)^2 / 730) 
    c = exp(-(Ls-295)^2 / 730)
    
    res =  a * (b + c)

    return(max(0.5, res))
    
  }else if (model == 2){
    
    a = 19500 * ((1 + phi/150) / (4000 + (phi+48.1)^2))
    b = exp(-(Ls-215)^2 / 730)
    c = 12700 * ((1 + phi/410) / (2465 + (phi+13.1)^2))
    d = exp(-(Ls-295)^2 / 730)
    
    res =  (a * b) + (c * d) 
    
    return(max(0.5, res))
    
  }else{
    stop("Unsupportd model number. Should be either 1 or 2.")
  }
  
}
