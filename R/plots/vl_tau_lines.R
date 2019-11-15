# Line plots of optical depth gound measurements data from the Viking Lander missions.
#
#   Dataset:      VL1/VL2 MARS LCS DERIVED ATMOSPHERIC OPTICAL DEPTH V1.0      
#   Description:  Viking Lander camera images of the Sun were used to compute total normal atmospheric
#                 optical depth at the two landing sites over a period of about 900 Mars days. 
#   Source:       PDS - Planetary Data System
#   URL:          https://pds.nasa.gov/ds-view/pds/viewProfile.jsp?dsid=VL1/VL2-M-LCS-5-ATMOS-OPTICAL-DEPTH-V1.0
#
# Citation Guideline:
#   https://pds.nasa.gov/datastandards/pds3/citing-pds3-data.shtml

library(here)
tau_lines = dget(here("functions/plots", "grouped_lines.R"))

vl_tau_lines = function(lander_id="VL1", sol_lim=c(0, 1147), include_years=NULL, sub=NULL, cols=NULL) {
  vl_data_url = "https://atmos.nmsu.edu/pdsd/archive/data/vl1vl2-m-lcs-5-atmos-optical-depth-v10/vl_1001/data/vl_opac.dat"
  
  if(!(toupper(lander_id) %in% c("VL-1", "VL1", "VL-2", "VL2", "Viking 1", "Viking 2", "Viking-1", "Viking-2"))){
    stop("Invalid input for lander_id, should either be 'VL-1' for Viking 1 or 'VL-2' for Viking 2.")
  }
  
  conn = file(vl_data_url, open="r")
  data_char = readLines(conn)
  close(conn)
  
  # Clean the data so that it is compliant with CSV format.
  # Put commas where there are whitespaces in between values.
  data_char = substring(gsub("\\s+", ",", data_char, perl=TRUE), 2)
  
  # Read as CSV.
  data = read.csv(text=data_char, header=FALSE)
  
  # Only grab data for the lander we are interested in.
  data = subset(data, V1==gsub("-", "", lander_id))
  
  Ls_vect = unlist(data[10])
  Sols = unlist(data[4])
  taus = unlist(data[7])
  
  # Plot lines.
  tau_lines(x=Ls_vect, y=taus, x_floor=0, x_ceil=360, i=Sols, ilim=sol_lim, cols=cols)
}