"
Use kml package on longitudinal cognitive measures and add groups to dataframes used in mixed models

Benefits of kml over standard algorithms:
  
  1. Enables modeling of joint trajectories (e.g. how memory and attention change over time together)
  2. Wide range of k selection methods

"
# import packages
library(kml)

# specify the dataframe we will be working with (created in select_longitudinal.R)

#df <- immediate_recall_df
#df <- animal_naming_df
df <- delayed_recall_df

# Scale data? is this inbuilt in KML?
# scaling the data is necessary for joint trajectory analysis (i.e. kml3d) because different measures might be on different scales, but it is not necessary for 
# non-joint analysis (i.e. kml). For now, we will follow an existing TILDA pipeline and not scale.

# create a matrix where each row corresponds to an individual trajectory

trajMatrix <- df[,-1]

# an ID vector the length og the trajectory matrix

IDlist <- df['tilda_serial']
IDnumbers <- seq.int(nrow(IDlist)) # lis of IDnumbers (as in celine analysis)

# specify the number of timepoints c(1:5) for 5 waves

timepoints <- c(1:5)

# create clusterLongData object 

clustObject <- clusterLongData(idAll = IDnumbers, time = c(1:5), traj = as.matrix(trajMatrix))

# run kml on clusterObject

kml(clustObject, nbClusters = 1:5, nbRedrawing = 100)  #If exploring (and want faster processing), remove or reduce the nbRedrawing. Takes around 7 mins with nbRedrawing 100

#Check the fit statistics - higher = better. Quality criteria comparison. Look for the number of clusters (x axis) that lead to the highest value on the y-axis. The criteria don't always agree.
plotAllCriterion(clustObject) 

# Plot the trajectories with the selected number of criterion. 
plot(clustObject,2) 
plot(clustObject,3) 
plot(clustObject,4) 
plot(clustObject,5) 


#Extract group-membership for each cluster IDs 
df_with_clusters <- data.frame(as.integer(clustObject@idAll), getClusters(clustObject,2), getClusters(clustObject,3), getClusters(clustObject,4), getClusters(clustObject,5))
#instead using tilda serial
df_with_clusters <- data.frame(IDlist, getClusters(clustObject,2), getClusters(clustObject,3), getClusters(clustObject,4), getClusters(clustObject,5))

colnames(df_with_clusters) <- c("tilda_serial", "nC2", "nC3", "nC4", "nC5")

