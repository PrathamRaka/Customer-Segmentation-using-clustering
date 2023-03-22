library(dplyr)
library(ggplot2)
library(purrr)
library(cluster) 
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)

df=read.csv("mall_customers.csv")
str(df)
names(df)
head(df)
summary(df$Age)
sd(df$Age)
summary(df$Annual.Income..k..)
sd(df$Annual.Income..k..)
summary(df$Age)
sd(df$Spending.Score..1.100.)

a=table(df$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")

hist(df$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

boxplot(df$Age,
        col="pink",
        main="Boxplot for Descriptive Analysis of Age")

summary(df$Annual.Income..k..)
hist(df$Annual.Income..k..,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

plot(density(df$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(df$Annual.Income..k..),
        col="#ccff66")

summary(df$Spending.Score..1.100.)

min 1st Qu. median mean 3rd Qu. max 
## 1.00 34.75 50.00 50.20 73.00 99.00

boxplot(df$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")

hist(df$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#6600cc",
     labels=TRUE)

ggplot(df, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
scale_color_discrete(name=" ", breaks=c("1", "2", "3", "4", "5","6"), labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

ggplot(df, aes(x =Spending.Score..1.100., y =Age)) + geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
scale_color_discrete(name=" ", breaks=c("1", "2", "3", "4", "5","6"), labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")
