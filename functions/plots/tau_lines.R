# Line plots of given optical depth data.
#
tau_lines = function(Ls_vect, Sols, taus, sol_start, sol_end, include_years, cols=NULL){
  Ls_yr = c()
  Sols_yr = c()
  taus_yr = c()
  
  Ls_prev = 999
  year_count = 0
  index = 1
  
  for(sol in Sols){
    if(sol >= sol_start && sol <= sol_end){
      # Handle negative values of Ls.
      Ls = ifelse(c(Ls_vect[index])<0, c(Ls_vect[index])+360, c(Ls_vect[index]))
      
      # New year, Gone from Ls=360-ish to Ls=0-ish.
      if(Ls < Ls_prev){
        
        if(!is.null(Ls_yr)){
          if(is.null(include_years) || year_count %in% include_years){
            lines(Ls_yr, taus_yr, type = "l",
                  col= if(is.null(cols)) "black" else cols[year_count+1])
          }
        }
        
        Ls_yr = c(Ls)
        Sols_yr = c(Sols[index])
        taus_yr = c(taus[index])
        
        year_count = year_count + 1
        
      }else{
        # Still in the same year.
        Ls_yr = c(Ls_yr, Ls)
        Sols_yr = c(Sols_yr, Sols[index])
        taus_yr = c(taus_yr, taus[index])
      }
      
      Ls_prev = Ls
      index = index + 1
    }
  }
}