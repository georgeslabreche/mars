# Calculate the Areocentric Longitude [deg] based on the given terrestial date.
#   See process here: https://www.giss.nasa.gov/tools/mars24/help/algorithm.html
#   Verified results against this: https://jtauber.github.io/mars-clock/
#
# Based on equations presented in the following publication:
#   Allison, M., and M. McEwen 2000. 
#   A post-Pathfinder evaluation of aerocentric solar coordinates with improved timing recipes for Mars seasonal/diurnal climate studies.
#   Planet. Space Sci. 48, 215-235.


# Make sure the time locale matches the Opportunity status data that will be read.
Sys.setlocale("LC_TIME", "C");

# The function.
#
# Paremeters:
#   date            - terrestial date. e.g. 9-Apr-1984.
#   format          - date pattern. e.g. %d-%b-%Y.
#   force_bounding  - force value to be beween 0 and 360 degrees.
function(date, format, force_bounding=FALSE){
  
  # Convert given date into date something that can be cast into a numeric.
  date = as.Date(date, format)
  date = as.POSIXlt(date, "%Y-%m-%d")
  
  # Milliseconds since Unix Epoch [ms].
  millis = as.numeric(date) * 1000
  
  # Unix time Julian date.
  JD_UT = 2440587.5 + (millis / 8.64e7)
  
  # Terrestial time Julian date.
  JD_TT = JD_UT + (37 + 32.184) / 86400
  
  # Days since J2000 Epoch [days].
  delta_t_J2000 = JD_TT - 2451545
  
  # Mars mean Anomaly [deg].
  M = (19.3871 + 0.52402075 * delta_t_J2000) %% 360
  M_rad = M * pi/180 # Convert to radian.s
  
  # The perturbers (pertubations caused by other planets and pluto) [deg].
  PBS = 
    0.0071 * cos((0.985626 * delta_t_J2000 /  2.2353) +  49.409) +
    0.0057 * cos((0.985626 * delta_t_J2000 /  2.7543) + 168.173) +
    0.0039 * cos((0.985626 * delta_t_J2000 /  1.1177) + 191.837) +
    0.0037 * cos((0.985626 * delta_t_J2000 / 15.7866) +  21.736) + 
    0.0021 * cos((0.985626 * delta_t_J2000 /  2.1354) +  15.704) +
    0.0020 * cos((0.985626 * delta_t_J2000 /  2.4694) +  95.528) +
    0.0018 * cos((0.985626 * delta_t_J2000 / 32.8493) +  49.095)
  
  # Equation of center [deg].
  nu = (M +
          (10.691 + 3e-7 * delta_t_J2000) * sin(M_rad) +
          0.623 * sin(2*M_rad) +
          0.05 * sin(3*M_rad) +
          0.005 * sin(4*M_rad) +
          0.0005 * sin(5*M_rad) +
          PBS) %% 360
  
  # Angle of fictitious mean sun [deg].
  alpha_FMS = (270.3863 + 0.52403840 * delta_t_J2000) %% 360
  
  # The Areocentric Longitude [deg].
  Ls = alpha_FMS + (nu - M)
  
  if(isTRUE(force_bounding)){
    # Handle negative Ls values (should not exist).
    if(Ls < 0){
      Ls = Ls + 360
      
    # Handle Ls values that are greater than 360 degrees.
    }else if(Ls > 360){
      Ls = Ls - 360
    }
  }
  
  return(Ls)
}