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

# Import packages
library(kml)
library(jcolors)# for plotting custom trajectories

# Type the measure you want to run k means on ('animal naming', 'delayed recall' or 'immediate recall')
this_cog_measure <- "animal naming"

# Functions
# Automatically save plots to a directory named figures/tables in the current working directory
current_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
path_breaks <- which(strsplit(current_directory, "")[[1]]=="/")
plot_outpath<- paste(substr(current_directory, start = 1, stop = path_breaks[length(path_breaks)]),'figures/kml_plots/', sep = '')

saveplot <- function(var, plot){
  ggsave(
    paste(plot_outpath, var,  '.pdf', sep = ''),
    plot = plot,
    device = NULL,
    path = NULL,
    scale = 1,
    width = 5,
    height = 6,
    units = c("in"),
    dpi = 300,
    limitsize = TRUE,
  )
}

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
#plot(clustObject,2) 
#plot(clustObject,3)
#plot(clustObject,4) 
#plot(clustObject,5) 


# Save the image of the three cluster plot 
saveplot(paste(this_cog_measure, '-kml', sep = ''), plot(clustObject,3))

# Extract group-membership for each cluster IDs 
df_with_clusters <- data.frame(as.integer(clustObject@idAll), getClusters(clustObject,2), getClusters(clustObject,3), getClusters(clustObject,4), getClusters(clustObject,5))
# Instead using tilda serial
df_with_clusters <- data.frame(IDlist, getClusters(clustObject,2), getClusters(clustObject,3), getClusters(clustObject,4), getClusters(clustObject,5))
# Make a data frame with the kmeans clusters
colnames(df_with_clusters) <- c("tilda_serial", "nC2", "nC3", "nC4", "nC5")

#### Relabel ####
# So that the following applied to all groups for interpretation
# A = healthiest
# B = mid
# C = least healthy 

df_with_clusters$nC3_orig <- df_with_clusters$nC3
if(this_cog_measure == "animal naming"){
  df_with_clusters$nC3_new <- ifelse(df_with_clusters$nC3 == "A", 'B',
                                     ifelse(df_with_clusters$nC3 == "B", 'C',
                                            ifelse(df_with_clusters$nC3 == "C", 'A', NA)))
  
}else if (this_cog_measure == "delayed recall"){
  df_with_clusters$nC3_new <- ifelse(df_with_clusters$nC3 == "A", 'B',
                                     ifelse(df_with_clusters$nC3 == "B", 'A',
                                            ifelse(df_with_clusters$nC3 == "C", 'C', NA)))
}else if (this_cog_measure == "immediate recall"){
  df_with_clusters$nC3_new <- ifelse(df_with_clusters$nC3 == "A", 'B',
                                     ifelse(df_with_clusters$nC3 == "B", 'A',
                                            ifelse(df_with_clusters$nC3 == "C", 'C', NA)))
  }
df_with_clusters$nC3 <- df_with_clusters$nC3_new
#### Custom K means plot ####

# Merge
cluster_data<-merge(df_with_clusters, df, by='tilda_serial')

if(this_cog_measure == "delayed recall"){
  varying_labels = c("COGdelayedrecall_W1", "COGdelayedrecall_W2", "COGdelayedrecall_W3", "COGdelayedrecall_W4", "COGdelayedrecall_W5")
  v_name = "delayed_recall"
  plot_title = "Delayed recall clusters"
  y_lab = "Words recalled (max 10)"
  print('Grouping based on delayed recall')
}else if(this_cog_measure == "animal naming"){
  varying_labels = c("COGanimal_naming_W1", "COGanimal_naming_W2", "COGanimal_naming_W3", "COGanimal_naming_W4", "COGanimal_naming_W5")
  v_name = "animal_naming"
  plot_title = "Animal naming clusters"
  y_lab = "Animals named (1 minute)"
  print('Grouping based on animal naming')
}else if(this_cog_measure == "immediate recall"){
  varying_labels = c("immediaterecall_total_W1", "immediaterecall_total_W2", "immediaterecall_total_W3", "immediaterecall_total_W4", "immediaterecall_total_W5")
  v_name = "immediate_recall"
  plot_title = "Immediate recall clusters"
  y_lab = "Words recalled (max 20)"
  print('Grouping based on immediate recall')
}

cluster_data_long<- reshape(cluster_data, idvar="tilda_serial",
                           varying = varying_labels,
                           v.name=c(v_name),
                           times=c("1", "2", "3", "4", "5"),
                           direction="long")

if(this_cog_measure == "delayed recall"){
  plt_1 <- ggplot(cluster_data_long, aes(x = time, y = delayed_recall, group = tilda_serial, color = nC3)) + geom_line(alpha = 0.02, size = 0.5)
}else if(this_cog_measure == "animal naming"){
  plt_1 <- ggplot(cluster_data_long, aes(x = time, y = animal_naming, group = tilda_serial, color = nC3)) + geom_line(alpha = 0.02, size = 0.5)
}else if(this_cog_measure == "immediate recall"){
  plt_1 <- ggplot(cluster_data_long, aes(x = time, y = immediate_recall, group = tilda_serial, color = nC3)) + geom_line(alpha = 0.02, size = 0.5)
}
plt_1 <- plt_1 +
  stat_summary(fun=mean, geom="line", aes(group=nC3),
               size=2.25) +
  facet_wrap(~ nC3) +
  ggtitle(plot_title) +
  labs(x = "TILDA wave", y = y_lab)+ theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(breaks = c("A", "B", "C"), 
                    values=c("#24a99c", "#c6cf6e", "#d7263d")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        strip.background = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), strip.text = element_text(size = 14, face = "bold"), 
        plot.title = element_text(size = 16, face = "bold"))

saveplot(paste(this_cog_measure, '-kml-sep', sep = ''), plt_1)

