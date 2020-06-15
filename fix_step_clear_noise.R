# 29.12.2018
#subject: clean noise in the data.
# clear_noise is a function that given a set of points (x0,y0), (x1,y1)...
# Filters out any points that had higher values,
# making the input data a monotonic linear graph.
clear_noise <- function(width){
  
  #create a vector that will hold the width results
  result_width <- c()
  
  # defined the minimal value for the first point
  local_max <- 0
  
  # Go through all the points and compare with the previously seen points
  # only points that are "higher" (or wider in our case) than previuosly seen points will be saved.
  for (i in 1:(length(width)-1)) {
    if(is.na(width[i])) {
      result_width <- append(result_width,NA)
      next()
    }
    
    if (width[i]>=local_max) {
      result_width <- append(result_width,width[i])
      local_max <- width[i]
    } else {
      result_width <- append(result_width,NA)
      }
    
  }  
  plot(result_width)
  return(list(width=result_width))
}