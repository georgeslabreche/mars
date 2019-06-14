# Line plots of optical depth gound measurements data from the Mars Exploration Rover missions (Spirit and Opportunity) .
#
#   Dataset:      Pancam Atmospheric Opacity Science RDRs
#   Description:  Mars Exploration Rover Pancam Atmospheric Opacity for both the MER-1 and MER-2 rovers.
#   Source:       PDS - Geosciences Node
#   URL:          http://pds-geosciences.wustl.edu/missions/mer/geo_mer_datasets.htm
#
# Citation Guideline:
#   https://pds.nasa.gov/datastandards/pds3/citing-pds3-data.shtml

library(here)
tau_lines = dget(here("functions/plots", "tau_lines.R"))

mer_tau_lines = function(rover_id="MER-A", sol_start=0, sol_end=5106, include_years=NULL, sub=NULL, cols=NULL) {
  mer_data_url = "http://pds-geosciences.wustl.edu/mer/mer1_mer2-m-pancam-5-atmos-opacity-v1/merao_1xxx/data/"
  
  # Opportunity, aka MER-B (Mars Exploration Rover – B) or MER-1.
  oppy_lbl_1 = paste(mer_data_url, "1tau440_5106_20181207a.lbl", sep="")
  oppy_tab_1 = paste(mer_data_url, "1tau440_5106_20181207a.tab", sep="")
  
  oppy_lbl_2 = paste(mer_data_url, "1tau880_5106_20181207a.lbl", sep="")
  oppy_tab_2 = paste(mer_data_url, "1tau880_5106_20181207a.tab", sep="")
  
  # Spirit, aka MER-A (Mars Exploration Rover – A) or MER-2.
  spirit_lbl_1 = paste(mer_data_url, "2tau440_2209_20110214a.lbl", sep="")
  spirit_tab_1 = paste(mer_data_url, "2tau440_2209_20110214a.tab", sep="")
  
  spirit_lbl_2 = paste(mer_data_url, "2tau880_2209_20110214a.lbl", sep="")
  spirit_tab_2 = paste(mer_data_url, "2tau880_2209_20110214a.tab", sep="")
  
  data_url = NULL
  if(toupper(rover_id) %in% c("MER-A", "MERA", "MER-2", "MER2", "SPIRIT")){
    data_url = spirit_tab_2
    
  }else if(toupper(rover_id) %in% c("MER-B", "MERB", "MER-1", "MER1", "OPPORTUNITY", "OPPY")){
    data_url = oppy_tab_2
    
  }else{
    stop("Invalid input for rover_id, should either be 'MER-A' for Spirit or 'MER-B' for Opportunity.")
  }
  
  conn = file(data_url, open="r")
  lines = readLines(conn)
  close(conn)
  
  # Clean the data so that it is compliant with CSV format.
  # Remove header text.
  data_char = lines[seq(10, length(lines), 1)]
  
  # Remove whitespace and quotation marks.
  data_char = gsub('\\s|\"', "", data_char, perl=TRUE)
  
  # Read as CSV.
  data = unname(read.csv(text=data_char, header=FALSE))
  
  Ls_vect = unlist(data[2])
  Sols = unlist(data[4])
  taus = unlist(data[7])
  
  tau_lines(Ls_vect, Sols, taus, sol_start, sol_end, include_years, cols=cols)
}