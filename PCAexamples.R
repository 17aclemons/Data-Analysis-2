library(ggplot)

irirs <- iris

pr.iris <- prcomp(x = iris[,-5], scale = TRUE, center = TRUE)
pr.iris$sdev
pr.var <- pr.iris$sdev^2
pve <- pr.var/sum(pr.var)
pve
pr.iris$rotation
pr.iris$x
biplot(pr.iris)
plot(pve, ylim = c(0,1), type = 
       'b')
ggplot(data.frame(pr.iris$x), aes(x = PC1, y = PC2,
       color = iris[,5])) + geom_point()
iris_scale <- scale(iris[,-5])
k_iris <- kmeans(x = iris_scale, centers = 2)
k_iris$cluster
k_iris$centers
k_iris$totss
k_iris$withinss
k_iris$tot.withinss
k_iris$betweenss
k_iris$size

model_withinss <- numeric(0)
for(i in 1:10){
  model <- kmeans(x = iris_scale, centers = i)
  model_withinss <- append(model_withinss, model$tot.withinss)
}
elbow_k <- data.frame(k = 1:10, withinss = model_withinss)

ggplot(elbow_k[-1,], aes(x = k, y = withinss)) + geom_line() + 
  scale_x_continuous(breaks = seq(0:10))

kmean2 <- kmeans(iris_scale, centers = 2, nstart = 20, iter.max = 20)
kmeans3 <- kmeans(iris_scale, centers = 3,
                  nstart = 20, iter.max = 20)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + scale_color_discrete(name = "Component")
ggplot(iris, aes(x = Sepal.length, ))

dist_matrix <- dist(iris_scale)
iris_hclust <- hclust(dist_matrix, method = "complete")
plot(iris_hclust0)
plot(hclust(dist_matrix, method = "centroid"))
plot(hclust(dist_matrix, method = "average"))
plot(hclust(dist_matrix, method = "ward.D"))

install.packages("dendextend")
install.packages("factoextra")
dendextend::find_k(iris_hclust)
#dendextend::find_k(hclust())
