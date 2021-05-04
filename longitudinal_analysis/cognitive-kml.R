"
Identify cognitive trajectory groups from waves 1 through to 5 of The Irish Longitudinal Study on Ageing (TILDA)
[Run following 'select_cross_sectional.R' and 'select_longitudinal.R']

Author(s): Hirst R J 

This analysis uses the kml package to identify cognitive trajectory groups with k-means clusterin of longitudinal data.


Benefits of kml over standard algorithms:
  
  1. Enables modeling of joint trajectories if needed (e.g. how memory and attention change over time together) - not used here
  2. Wide range of k selection methods and cluster validation
  3. Clear documentation and publications

"
# Type the measure you want to run k means on ('animal naming', 'delayed recall' or 'immediate recall')
this_cog_measure <- "immediate recall"

# Import packages
library(kml)

# Specify the dataframe based in this_cog_measure (created in select_longitudinal.R)
if(this_cog_measure == "animal naming"){
  df <- animal_naming_df
}else if(this_cog_measure == "delayed recall"){
  df <- delayed_recall_df
}else if (this_cog_measure == "immediate recall"){
  df <- immediate_recall_df
}else{warning(paste(this_cog_measure, " is not a group please use 'animal naming', 'delayed recall' or 'immediate recall'"))}

# Create a matrix where each row corresponds to an individual trajectory
trajMatrix <- df[,-1]

# An ID vector the length og the trajectory matrix
IDlist <- df['tilda_serial']
IDnumbers <- seq.int(nrow(IDlist)) # list of IDnumbers

# Specify the number of timepoints c(1:5) for 5 waves
timepoints <- c(1:5)

# Create clusterLongData object 
clustObject <- clusterLongData(idAll = IDnumbers, time = c(1:5), traj = as.matrix(trajMatrix))

# Run kml on clusterObject
kml(clustObject, nbClusters = 1:5, nbRedrawing = 100)  # If exploring (and want faster processing), remove or reduce the nbRedrawing. Takes around 7 mins with nbRedrawing 100

# Use choice to see the number of clusters selected based on Calsinki and Harabatz 
choice(clustObject)

# Check consistency across several fit statistics
# higher = better. Quality criteria comparison. Look for the number of clusters (x axis) that lead to the highest value on the y-axis. 
# The criteria don't always agree - pick the most consistent.
plotAllCriterion(clustObject) 

# Plot the trajectories with the selected number of criterion. 
plot(clustObject,2) 
plot(clustObject,3) 
plot(clustObject,4) 
plot(clustObject,5) 


# Extract group-membership for each cluster IDs 
df_with_clusters <- data.frame(as.integer(clustObject@idAll), getClusters(clustObject,2), getClusters(clustObject,3), getClusters(clustObject,4), getClusters(clustObject,5))
# Instead using tilda serial
df_with_clusters <- data.frame(IDlist, getClusters(clustObject,2), getClusters(clustObject,3), getClusters(clustObject,4), getClusters(clustObject,5))
# Make a data frame with the kmeans clusters
colnames(df_with_clusters) <- c("tilda_serial", "nC2", "nC3", "nC4", "nC5")

