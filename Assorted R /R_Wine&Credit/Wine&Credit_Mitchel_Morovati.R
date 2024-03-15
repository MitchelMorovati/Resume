rm(list=ls()) 

library(ggplot2)                
library(cluster)                 
library(factoextra)

# read wine data
wine <- read.csv('data_wine.csv',header = TRUE, sep = ',',stringsAsFactors = TRUE)

str(wine)

# data preparation
#check if there is missing values in the data
sum(is.na(wine))

#check data types
str(wine)

km_wine <- scale(wine)

km2 <- kmeans(km_wine, centers = 2, nstart = 20)
km2

km2$cluster

# vizualize cluster
fviz_cluster(km2,data = km_wine) + theme_bw()+ggtitle('Two Clusters')

#optimal clusters
# decide the optimal number of clusters
# look at the plot to determine the optimal number of clusters
fviz_nbclust(km_wine,kmeans,method = 'wss')
# find the #clusters such that maximize the avg silhouette width
fviz_nbclust(km_wine,kmeans,method = 'silhouette')
#clustering with 3 clusters
km3 <- kmeans(km_wine, centers = 3, nstart = 20)

km3$size

fviz_cluster(km3,data = km_wine) + theme_bw()+ggtitle('Three Clusters')

###################################################################################
#read credit card data
cards <- read.csv('data_credit_card.csv',header = TRUE, sep = ',',stringsAsFactors = TRUE)

sum(is.na(cards))

str(cards)

new_cards <- subset(cards, select = -c(id,age,line_limit))
km_cards <- scale(new_cards)

# calculate dissimilarity matrix
dis_matrix <- dist(km_cards, method =  'euclidean')

# hierarchical clustering using ward linkage
hc_ward <- hclust(dis_matrix, method = 'ward.D2')
#png("hierarchical_clustering_plot.png", width = 800, height = 600)
plot(hc_ward, hang = -1, cex = 0.5)
#dev.off()  # Close the PNG device

####################################################################################
#kmeans clustering
km2 <-kmeans(km_cards, centers = 2, nstart = 20)
fviz_cluster(km2,data = km_cards) + theme_bw()+ggtitle('Two Clusters')
  
km3 <-kmeans(km_cards, centers = 3, nstart = 20)
fviz_cluster(km3,data = km_cards) + theme_bw()+ggtitle('Three Clusters')
#2 cluster mean and median
cards$Cluster <- km2$cluster
cluster2_stats <- aggregate(cbind(age, line_limit) ~ Cluster, data = cards, FUN = function(x) c(mean = mean(x), median = median(x)))
print(cluster2_stats)



