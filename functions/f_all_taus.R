library(here)

f_build_df = dget(here("functions", "f_build_df.R"))

# The function.
#
#   nfft    - Net flux function implementation type.
#               - 1 for f_89.
#               - 2 for f_90.
#               - 3 for f_analytical.
function(nfft){
  if(nfft==1){
    as.numeric(rownames(f_build_df(0.1, 1989)))
    
  }else if(nfft==2){
    as.numeric(rownames(f_build_df(0.1, 1990)))
    
  }else{
    stop("Unsupported net flux type, should be 1 for the original 1989 publication or 1990 for the 1990 update")
  }
}