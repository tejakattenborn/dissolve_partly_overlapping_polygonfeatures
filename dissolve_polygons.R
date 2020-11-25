################################################################################################################
#author : Teja Kattenborn     mail:teja.kattenborn@uni-leipzig.de     twitter: @TejaKattenborn


# This script was build to prostprocess Instance Segmentaion output from a CNN (Mask-R-CNN) in the context of dead tree detection.

# This script aims to dissolve/aggregate partly overlapping features in a polygon shapefile.
# The procedure iterates over the features of a polygon and identifies features pairs with a distance smaller than 'distance_threshold'
# For these features, the intersection is calculated and an aggregation is performed if the intersection exceeds 'intersect_threshold'

################################################################################################################


# load packages
require(rgdal)
require(rgeos)
library(sp)
require(raster)

# set workspace & load data
setwd("D:/googledrive/data_confobi_deadwood/mask_r_cnn_output/pred_2016_benchmark/")
trees = readOGR(dsn = getwd(), layer = "CFB007_ORTHO_RGB_2016_shp")
trees = gBuffer(trees, byid=TRUE, width=0)

# only consider lying trees
trees_l = trees[which(trees$class==0),]


##filter features by minum size (reduces computation time)
#calc area for all trees
trees_l_area = c()
for(i in 1:length(trees_l)){
  trees_l_area[i] = gArea(trees_l[i,])
}
trees_l = trees_l[which(trees_l_area > 0.01),]

# calculate centre point of feature (their are way more elegant solutions)
trees_l_bbox = matrix(NA,ncol=4, nrow=nrow(trees_l))
colnames(trees_l_bbox) = c("xmin", "xmax", "ymin", "ymax")
for(i in 1:nrow(trees_l_bbox)){
  trees_l_bbox[i,] = as.vector(trees_l[i,]@bbox)
}
trees_l_centre = matrix(NA,ncol=2, nrow=nrow(trees_l_bbox))
trees_l_centre[,1] = trees_l_bbox[,1]- trees_l_bbox[,2]
trees_l_centre[,2] = trees_l_bbox[,3]- trees_l_bbox[,4]


# set thresholds
intersect_threshold = 0.4 # fraction of overlap
distance_threshold = 7 # meters of target distance

# preparation for while-condition
combinations = t(combn(1:length(trees_l), 2))
i = 1
i_0 = 1

while(i != nrow(combinations)){
  
  ID1 = combinations[i,1]
  ID2 = combinations[i,2]
  i = i+1
  
  
  #if distance is below threshold, check overlap and merge if overlap is high enough
  distance = sqrt(abs(trees_l_centre[ID1,1] - trees_l_centre[ID2,1])^2 + abs(trees_l_centre[ID1,2] - trees_l_centre[ID2,2])^2)
  if(distance < distance_threshold){
    
    #if features intersect merge and remove old featuees
    intersect_val = gIntersection(trees_l[ID1,], trees_l[ID2,])
    if(length(intersect_val) == 1){
      
      area_intersect = gArea(gIntersection(trees_l[ID1,], trees_l[ID2,]))
      if(area_intersect/gArea(trees_l[ID1,]) > intersect_threshold || area_intersect/gArea(trees_l[ID2,]) > intersect_threshold){
        
        poly_cominbed = aggregate(trees_l[c(ID1,ID2),], dissolve = TRUE, FUN=mean)
        poly_cominbed$ID <- NULL
        trees_l = trees_l[-c(ID1, ID2),]
        trees_l = rbind(trees_l,poly_cominbed)
        
        combinations = t(combn(1:length(trees_l), 2))
        i = 1
        
        print("Overlap detected! Iteration restarted...")
        flush.console()
        
      }
    }
  }
  
  #print progress
  if(i_0 < i){
    i_0 = i
    print(paste0("combination ", i , " of ", nrow(combinations)))
    flush.console()
  }
}


# export new shapefile
shapefile(x = trees_l, file = "test123.shp")
