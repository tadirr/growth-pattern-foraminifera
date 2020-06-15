#19.12.2017
# subject: specimen measuring
# This function measures each picture once processed and outputs the measurments as a vector
# Each measurment is taken at a fixed step size of 10 Microns.
measuring_specimen_width<-function(image,metadata_path){
  
  #defined image as matrix
  matrix <- as.matrix(image)
  
  # call function (scale)that give the px/um ratio
  # scale um is no. of pix that equal to 1 um
  source('C:/Users/user/Dropbox/PhD/P.nuttalli/XML_parse.R')
  scale_um <- scale(metadata_path)
  
  #defined step size as 10 Microns in pix
  step_size <- 10*scale_um
  
  #Defining the no. of messurment (total length divided into step size)
  num_measurements <- as.integer(floor(dim(matrix)[2]/step_size))
  
  #creating vector that will contain the no. of required width messurments
  results<-numeric(num_measurements)
  
  #creating vector that will contain the logarithmic variables
  log_parameters<-vector()
  
  #creating vector that will contain the highet at each step size
  heights <- numeric(num_measurements)
  
  #creating vector that will contain the first white at each step size
  curve <- numeric(num_measurements)
  
  # The way we measure the width (in pixels) of the image in a certain height
  # is by substracting the first white pixel from the last white pixel.
  # The reason for that is that sometimes images can have noise which will produce
  # incorrect widths.
  
  # loop that add steps size
  for(i in 1:num_measurements) {
    r<-i*step_size
    
    # put heights results of each step size into vector
    heights[i] <- r
    
    # defined the first column
    c_i <- as.integer(1)
    
    # go through each step size (row) on all columns, from one until white (value "1") 
    while( matrix[c_i,r] < 1){
      # when white go to the next step size
      c_i = c_i + 1
    }
    
    # defined the last column
    c_f <- as.integer(dim(matrix)[1])
    
    # go down through each step size (row) on all columns, from the last one until white (value "1") 
    while( matrix[c_f,r] < 1){
      # when white go to the previous step size
      c_f = c_f - 1
    }
    
    #curve is the length of the image until the first white at each step size
    curve[i] <- c_i
    
    # defind the length of the white columns at each step size according to first and last white column
    width <- c_f-c_i
    
    # put width results from each step size into vector
    results[i] <- width
  }
  
  
  # the red lines represent all messurments
  plot(image)
  abline(h=heights,col="red")

  #print(results)
  return(results)
 
}

