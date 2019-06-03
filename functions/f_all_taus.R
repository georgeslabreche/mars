f_build_df = dget("functions/f_build_df.R")

function(){
  as.numeric(rownames(f_build_df()))
}