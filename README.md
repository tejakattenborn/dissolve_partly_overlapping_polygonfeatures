# dissolve_partly_overlapping_polygonfeatures

This script was build to prostprocess Instance Segmentaion output from a CNN (Mask-R-CNN) in the context of dead tree detection.

This script aims to dissolve/aggregate partly overlapping features in a polygon shapefile. The procedure iterates over the features of a polygon and identifies features pairs with a distance smaller than 'distance_threshold' For these features, the intersection is calculated and an aggregation is performed if the intersection exceeds 'intersect_threshold'
