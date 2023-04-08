install.packages("factoextra")
library(ggplot2)
library(factoextra)
library(purrr)

setwd("/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo")

file_name <- "clustering-data.csv"
cluster1 <- read.csv(file_name)
head(cluster1)

ggplot(cluster1, aes(x=x, y=y)) + 
  geom_point()

set.seed(42)

cluster2 <- kmeans(cluster1, 2)
str(cluster2)
fviz_cluster(cluster2, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster2$cluster))) + 
         geom_point()



cluster3 <- kmeans(cluster1, 3)
str(cluster3)
fviz_cluster(cluster3, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster3$cluster))) + 
  geom_point()

cluster4 <- kmeans(cluster1, 4)
str(cluster4)

fviz_cluster(cluster4, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster4$cluster))) + 
  geom_point()

cluster5 <- kmeans(cluster1, 5)
str(cluster5)
fviz_cluster(cluster5, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster5$cluster))) + 
  geom_point()

cluster6 <- kmeans(cluster1, 6)
str(cluster6)
fviz_cluster(cluster6, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster6$cluster))) + 
  geom_point()

cluster7 <- kmeans(cluster1, 7)
str(cluster7)
fviz_cluster(cluster7, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster7$cluster))) + 
  geom_point()

cluster8 <- kmeans(cluster1, 8)
str(cluster8)
fviz_cluster(cluster8, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster8$cluster))) + 
  geom_point()

cluster9 <- kmeans(cluster1, 9)
str(cluster9)
fviz_cluster(cluster9, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster9$cluster))) + 
  geom_point()

cluster10 <- kmeans(cluster1, 10)
str(cluster10)
fviz_cluster(cluster10, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster10$cluster))) + 
  geom_point()

cluster11 <- kmeans(cluster1, 11)
str(cluster11)
fviz_cluster(cluster11, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster11$cluster))) + 
  geom_point()

cluster12 <- kmeans(cluster1, 12)
str(cluster12)
fviz_cluster(cluster12, data = cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(cluster1, aes(x=x, y=y, colour = as.factor(cluster12$cluster))) + 
  geom_point()


cluster2$withinss
cluster3$withinss
cluster4$withinss
cluster5$withinss
cluster6$withinss
cluster7$withinss
cluster8$withinss
cluster9$withinss
cluster10$withinss
cluster11$withinss
cluster12$withinss



tot_withinss <- map_dbl(2:12,  function(k){
  model <- kmeans(x = cluster1, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 2:12,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 2:12)

