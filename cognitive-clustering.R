"
Practice kmeans clustering on longitudinal cognitive measures

"
library(stats) # for kmeans
library(factoextra) # for creating plots of clusters
library(cluster)
library(GGally)
library(plotly)
library(clValid)# for cluster validation

#### functions ####

# plot the within sum of squares for a number of executions of the k-means algorithm
# wss = how dissimilar the individuals in a group are - so smaller is better
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

#### prepare dataframe ####
# thisDF = a dataframe containing one column with tilda_Serial and the remaining the cognitive measure recorded across n waves
thisDf = animal_naming_df
# set the serial number as the row names
thisDf2 <- thisDf[,-1]
rownames(thisDf2) <- thisDf[,1]

# scale the data
df <-scale(thisDf2)
df2<-as.data.frame(scale(thisDf2))
df<-df2

# check if there are any NA of NaN values in the dataframe (clValid will not know how to cluster)
sum(is.nan(df))
sum(is.na(df))
df <- df[complete.cases(df), ]

#### Run clustering ####

# set seed to allow replication
set.seed(123)

# run clustering across a range of K and algorithms to identify optimal clustering approach\

#internal validity: Connectvity, Dunns Index, Sillhoette
intern<- clValid(df, 2:6, clMethods=c("kmeans", "pam", "hierarchical"), validation = "internal")
summary(intern)

# plot the measures of internal validity to compare models
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(intern, legend=FALSE)
plot(nClusters(intern),measures(intern,"Dunn")[,,1],type="n",axes=F,
     xlab="",ylab="")
legend("center", clusterMethods(intern), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

#### Evaluate optimal clustering ####
optimal_k <- 3

#### Hierarchical clustering ####

# plot the cluster dendogram (if hierarchical clustering outperforms other algorithms)
hc <- clusters(intern, "hierarchical")
# cut the tree at the optimal n clusters
cut_avg <- cutree(hc, k = optimal_k )

# color the branches so that we can see the individual clusters
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hc)
avg_col_dend <- color_branches(avg_dend_obj, k = optimal_k )
plot(avg_col_dend)

# Append the cluster results back onto the original data frame for interpretation 
suppressPackageStartupMessages(library(dplyr))
df_cl <- mutate(df, cluster = cut_avg)
count(df_cl$cluster)



# plot the trajectories for each cluster 

# Create a group-means data frame (gd)
gd <- df_cl %>% 
  group_by(cluster) %>% 
  summarise(
    immediaterecall_total_W1 = mean(immediaterecall_total_W1),
    immediaterecall_total_W2= mean(immediaterecall_total_W2),
    immediaterecall_total_W3= mean(immediaterecall_total_W3),
    immediaterecall_total_W4= mean(immediaterecall_total_W4),
    immediaterecall_total_W5= mean(immediaterecall_total_W5),
  )



library(gridExtra)
p1 <- ggplot(df_cl, aes(x = cluster, y = immediaterecall_total_W1, fill = paste(cluster))) +
  geom_point()+
  geom_bar(data = gd, stat = "identity", alpha = .3)

p2 <- ggplot(df_cl, aes(x = cluster, y = immediaterecall_total_W2, fill = paste(cluster))) +
  geom_point()+
  geom_bar(data = gd, stat = "identity", alpha = .3)

p3 <- ggplot(df_cl, aes(x = cluster, y = immediaterecall_total_W3, fill = paste(cluster))) +
  geom_point()+
  geom_bar(data = gd, stat = "identity", alpha = .3)

p4 <- ggplot(df_cl, aes(x = cluster, y = immediaterecall_total_W4, fill = paste(cluster))) +
  geom_point()+
  geom_bar(data = gd, stat = "identity", alpha = .3)

p5 <- ggplot(df_cl, aes(x = cluster, y = immediaterecall_total_W5, fill = paste(cluster))) +
  geom_point()+
  geom_bar(data = gd, stat = "identity", alpha = .3)



pp<-grid.arrange(p1, p2,p3,p4, p5, nrow = 2, widths=c(2, 2, 2))

hc_df<-df_cl
# note: would access the cluster labels for kmeans or PAM using intern@clusterObjs$kmeans$`2` (for 2 clusters)
# hierarchical clustering method uses hclust https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/hclust

# stability validation 

stab <- clValid(df, 2:6, clMethods=c("kmeans","pam", "hierarchical"),
                validation="stability")

#### Plot and evaluate k-means clustering ####
k_df<-df
k_df$cluster <- as.factor(intern@clusterObjs$kmeans$`2`$cluster)
fviz_cluster(intern@clusterObjs$kmeans$`2`, df, ellipse.type = "euclid")

df_cl<-k_df


