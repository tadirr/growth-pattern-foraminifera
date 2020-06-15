# 5.1.2019
# Subject: Ps.nuttalli growth pattern according to biozone - fix steps
library(ggplot2)

edge_view <- read.csv('C:/Users/user/Dropbox/Roni_PHD/P.nuttalli/fix_step_edge_view_clean.csv')
side_view <- read.csv('C:/Users/user/Dropbox/Roni_PHD/P.nuttalli/fix_step_side_view_clean.csv')

# create  seperate tables for each biozone
nut_edge_maya <- edge_view[which(edge_view$species=='P.nuttalli'&(edge_view$BZ=="A.mayaroensis")),]
nut_side_maya <- side_view[which(side_view$species=='P.nuttalli'&(side_view$BZ=="A.mayaroensis")),]

nut_edge_asym <- edge_view[which(edge_view$species=='P.nuttalli'&(edge_view$BZ=="D.asymetrica")),]
nut_side_asym <- side_view[which(side_view$species=='P.nuttalli'&(side_view$BZ=="D.asymetrica")),]

nut_edge_elev <- edge_view[which(edge_view$species=='P.nuttalli'&(edge_view$BZ=="G.elevata")),]
nut_side_elev <- side_view[which(side_view$species=='P.nuttalli'&(side_view$BZ=="G.elevata")),]

nut_edge_palp <- edge_view[which(edge_view$species=='P.nuttalli'&(edge_view$BZ=="P.palpebra")),]
nut_side_palp <- side_view[which(side_view$species=='P.nuttalli'&(side_view$BZ=="P.palpebra")),]

nut_edge_mas_pacific <- edge_view[which(edge_view$species=='P.nuttalli'&(edge_view$Location=="Pacific")&(edge_view$BZ=="P.palpebra" | edge_view$BZ=="A.mayaroensis")),]
nut_side_mas_pacific <- side_view[which(side_view$species=='P.nuttalli'&(side_view$Location=="Pacific")&(side_view$BZ=="P.palpebra" | side_view$BZ=="A.mayaroensis")),]

nut_edge_mas_tethys <- edge_view[which(edge_view$species=='P.nuttalli'&(edge_view$Location=="Tethys")&(edge_view$BZ=="P.palpebra" | edge_view$BZ=="A.mayaroensis")),]
nut_side_mas_tethys <- side_view[which(side_view$species=='P.nuttalli'&(side_view$Location=="Tethys")&(side_view$BZ=="P.palpebra" | side_view$BZ=="A.mayaroensis")),]

nut_edge_mas_atlantic <- edge_view[which(edge_view$species=='P.nuttalli'&(edge_view$Location=="Atlantic")&(edge_view$BZ=="P.palpebra" | edge_view$BZ=="A.mayaroensis")),]
nut_side_mas_atlantic <- side_view[which(side_view$species=='P.nuttalli'&(side_view$Location=="Atlantic")&(side_view$BZ=="P.palpebra" | side_view$BZ=="A.mayaroensis")),]


plot_shaded_area_graph<-function(data1,colour,y_max,y_lable) {
  height <- c()
  max_lable <- c()
  min_lable <- c()
  
  # calculate min, max and average valuse of data 1 
  average_values1 <- c()
  min_values1 <- c()
  max_values1 <- c()
  
  for (i in 1:length(data1)){
    height<-append(height,i*10)
    average_values1 <- append(average_values1,mean(data1[,i],na.rm = TRUE))
    min_values1 <- append(min_values1,(mean(data1[,i],na.rm = TRUE)-sd(data1[,i],na.rm = TRUE)))
    max_values1 <- append(max_values1,(mean(data1[,i],na.rm = TRUE)+sd(data1[,i],na.rm = TRUE)))
    max_lable <- append(max_lable,"max")
    min_lable <- append(min_lable,"min")
  }
  
  is.na(min_values1) <- sapply(min_values1, is.infinite)
  is.na(max_values1) <- sapply(max_values1, is.infinite)
  values1 <- append(rev(min_values1),max_values1)
  height2 <- append(rev(height),height)
  group <- append(min_lable,max_lable)
  
  df1 <- data.frame(height2,values1,group)
  names(df1) <- c('Height',"values (um)","group")
  
  df1.2 <- data.frame(height,average_values1)
  
   #plot
  ggplot(df1, aes(x = height2, y = values1))+
    xlab("Height (um)")+ ylab(y_lable)+ 
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ylim(0,y_max)+
    geom_polygon(aes(y = values1,x=height2),colour="grey27",fill=colour)+
    geom_line(data=df1.2,aes(y=average_values1,x=height),size=1,colour='black')
}