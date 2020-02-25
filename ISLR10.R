#Andrew Clemons
#ISLR 10
library(ggplot2)
MaintainabilityResearch <- read.csv("C:/Users/17acl/Downloads/MaintainabilityResearch.csv", stringsAsFactors=FALSE)


####1####
#Perform principal components analysis on the data. Comment on these components. Which ones are most relevant? 
#What are your ideas on what they mean? 
#You have an expert code auditor on staff to provide the results of your principal components analysis. 
#Write an email in the text box of your submission explaining the next steps on what they need to do with your results
#and how they could be use

#cleaning
df<- MaintainabilityResearch
df <- as.data.frame(sapply(df, as.numeric))
use <- complete.cases(df)
df <- df[use,]
which(apply(df, 2, var) == 0)
df <- df[-2:-4, c(-1:-4, -23,-51)]
#PCA 
pr.out <- prcomp(df, scale. = TRUE, center = TRUE)
plot(pr.out)
pr.var <- pr.out$sdev ^2
pve <- pr.var/sum(pr.var)
biplot(pr.out)
plot(cumsum(pve), ylim = c(0,1), type = 'b')


#Princple component 1 describes the largest amount of the data, PC 2 and 3 describe some but not a significant amount of the data.
#PC 1 to 3 show that a large amount of the data can be explained in relatively few variables and should be used to prevent overfitting
#with such a large dataset. 
####2####
#Create a cluster analysis. Use the type of cluster analysis that would be most appropriate for this type of data.
df.scale <- scale(df)
model_w <- numeric(0)
for(i in 1:10){
  model <- kmeans(x = df, centers = i)
  model_w <- append(model_w, model$tot.withinss)
}
elbowk <- data.frame(k = 1:10, withinss = model_w)

ggplot(elbowk[-1,], aes( x = k, y = withinss))

#If I could get my data to scale properly, I would use a complete hierarchial clustering to visualized my data as it leads
#to better balanced dendrograms and the results are easier to visualize. 
