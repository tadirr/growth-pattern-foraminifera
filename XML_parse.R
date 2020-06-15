#20.12.2017
# adding scale to image - converting from Pixels to microns based on data files
# creating a function that returns the scale (pixel/um) of image from XML data file 
scale <- function(path){

  # uplowding 'XML packege'
  library(XML)
  
  xmlfile <- xmlTreeParse(path)
  
  #gives content of root
  xmltop = xmlRoot(xmlfile)
  
  # go thrwo children nodes one by one
  elem<-xmltop[["Element"]]
  
  data<-elem[["Data"]]
  
  image<-data[["Image"]]
  
  image_desc <- image[["ImageDescription"]]
  
  dimensions <- image_desc[["Dimensions"]]
  
  # extract length in meters
  meter <- as.numeric(xmlAttrs(dimensions[["DimensionDescription"]])[["Length"]])
  
  micrometer <- meter*10^6
  
   # extract length in pixeles
  pixel <- as.numeric(xmlAttrs(dimensions[["DimensionDescription"]])[["NumberOfElements"]])
 
   scale <- pixel / micrometer
  
  return(scale)
  
}

