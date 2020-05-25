#install.packages('corrplot')
library(corrplot)
# Clustering
library(cluster) 

data <- read.csv('Wholesale customers data.csv', header = TRUE)
head(data,5)

data <- na.omit(data)
str(data)

summary(data)


corrmatrix <- cor(data)
corrplot(corrmatrix, method = 'number')

df <- data[-c(1,2)]
head(df,3)

silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

km.final <- kmeans(df, 7)
## Total Within cluster sum of square
km.final$tot.withinss

## Cluster sizes
km.final$size

data$cluster <- km.final$cluster
head(data, 6)

clusplot(data, data$cluster, color=TRUE, shade = TRUE, label=2)
  
