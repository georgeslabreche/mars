library(here)

f_build_df = dget(here("functions", "f_build_df.R"))

function(nfft){
  if(nfft==1){
    as.numeric(gsub("X", "", colnames(f_build_df(0.1, 1989))))
    
  }else if(nfft==2){
    as.numeric(gsub("X", "", colnames(f_build_df(0.1, 1990))))
    
  }else{
    stop("Unsupported net flux type, should be 1 for the original 1989 publication or 1990 for the 1990 update")
  }
}


