#24/12/2017
#Subject: going through all files image by image and produce data.

# create a matrix for measurements of all images
# divided based on image orientation: edge view or side view
side_view_data <- matrix(ncol = 81,nrow = 0,byrow = TRUE)
edge_view_data <- matrix(ncol = 81,nrow = 0,byrow = TRUE)


# padding is adding "Na" at the end of each row in order to fill the row 
padding_row <- function(input_row,limit) {
  
  #create a loop that will add NA antil the end of the row
  for (i in (1+length(input_row)):limit) {
    input_row <- append(input_row,NA)
  }
  return(input_row)  
}  


#create a matrix for maximal dimensions measurements
dim_side_view <- matrix(ncol = 3,nrow=0,byrow = TRUE)
dim_edge_view <- matrix(ncol = 3,nrow=0,byrow = TRUE)

# project is a list of files: 'Israel','leg_207', 'Leg_32' 
project <- list.files('E:/Image_processing')

# create a loop that goes through the files (sites) in the directory
for (sites in project) {
  
  # site_path is just the path of the folders underneath image processing 
  # e.g. first one will be: 'C:/Users/roni/Desktop/Image_processing/Israel'
  # second one will be: 'C:/Users/roni/Desktop/Image_processing/Leg_207'
  # etc...
  site_path <- paste('E:/Image_processing/',sites, sep='')
  
  # specimens is a list of individuals from a given site
  specimens <- list.files(site_path)
  
  # create a loop that goes through the files (specimens) in the directory
  for (specimen in specimens) {
    specimen_path <- paste(site_path,"/",specimen,sep = '')
    # for each specimen create two pathes acording to image orientation
    # either side_view or edge_view
    side <- paste(specimen_path,'/','Side_view',sep = '')
    edge <- paste(specimen_path,'/','Edge_view',sep = '')
    
    # side_image_name is a list of all jpg image files within this folder
    # in this folder thare is only one image
    # side_image_path is the path for this image
    side_image_name <- list.files(side,pattern = "\\.jpg$")
    side_image_path <- paste(side,'/',side_image_name,sep = '')
    
    # side_metadata_name is a list  of all XLIF files within this directory that ends with "Multifocus Image"
    # in this folder thare are three data files, but only one with this ending
    # therefor only one file within this list 
    # side_metadata_path is the path for the data file
    side_metadata <- paste(side,'/','leicametadata',sep='')
    side_metadata_name <- list.files(side_metadata,pattern = '\\Multifocus Image.xlif')
    side_metadata_path <- paste(side_metadata,'/',side_metadata_name,sep = '')
    
    # similar treatment to 'side_view' folder, at the end two pathes are given: 
    # edge_image_path for the image file
    # edge_metadata_path for the coresponding data file
    edge_image_name <- list.files(edge,pattern = "\\.jpg$")
    edge_image_path <- paste(edge,'/',edge_image_name,sep = '')
    
    edge_metadata <- paste(edge,'/','leicametadata',sep='')
    edge_metadata_name <- list.files(edge_metadata,pattern = '\\Multifocus Image.xlif')
    edge_metadata_path <- paste(edge_metadata,'/',edge_metadata_name,sep = '')
    
    # call function (image_processing) that give the measurement within each image in micrones
    source('C:/Users/user/Dropbox/Roni_PHD/P.nuttalli/image processing.R')
    side_measurements <- image_processing(side_image_path,side_metadata_path)
    side_measurements <- padding_row(side_measurements,80)
    side_mes_with_name <- append(side_measurements,specimen)
    print(side_mes_with_name)
   
     side_view_data <- rbind(side_view_data, side_mes_with_name)
    # dim_side_view <- rbind(dim_side_view, side_mes_with_name)
    
    edge_measurements <- image_processing(edge_image_path,edge_metadata_path)
    edge_measurements <- padding_row(edge_measurements,80)
    edge_mes_with_name <- append(edge_measurements,specimen)
    print(edge_mes_with_name)
    edge_view_data <- rbind(edge_view_data, edge_mes_with_name)
    #dim_edge_view <- rbind(dim_edge_view, edge_mes_with_name)
    }
  
}

# colnames give each column a name from 1 to 80
#colnames(dim_side_view) <- c('width','height(um)','foram_name')
#colnames(dim_edge_view) <- c('depth','height(um)','foram_name')
write.csv(side_view_data,'C:/Users/user/Dropbox/Roni_PHD/P.nuttalli/fix_step_side_view.csv')
write.csv(edge_view_data,'C:/Users/user/Dropbox/Roni_PHD/P.nuttalli/fix_step_edge_view.csv')


  