library(arules)
library(arulesViz)
library(ggplot2)
library(standardize)
library(stats)
library(caret)
library(clv)
library(klaR)
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)

setwd("/Users/vascofernandes/Desktop/projSAD")

## noshow dataset
noshow_dataset <- read.csv("noshows.20171013.csv")

## crabs dataset exploring data
crabs_dataset <- read.csv("crabs.csv")
head(crabs_dataset)
summary(crabs_dataset)
str(crabs_dataset)

## association rules - crabs dataset
## pre processing
for (i in 3:8){crabs_dataset[,i]<-discretize(crabs_dataset[,i])}
## association rules - noshow dataset
rules_crabs <- apriori(crabs_dataset, parameter = list(supp = 0.15, conf = 0.85, target = "rules"));
inspect(rules_crabs);
inspect(sort(rules_crabs, by="lift"));
plot(sort(rules_crabs, by="lift"))

## association rules - noshow dataset

## pre processing
library(readr)
noshows_20171013 <- read_delim("noshows.20171013.csv",";", escape_double = FALSE, trim_ws = TRUE)
install.packages('e1071', dependencies=TRUE)
require(caret)
noshows_20171013$PatientId <- NULL
noshows_20171013$AppointmentID <- NULL
noshows_20171013$ScheduledDay <- NULL
noshows_20171013$AppointmentDay <- NULL
noshows_20171013$Neighbourhood <- NULL
noshows_20171013$`No-show` <- NULL
noshows_20171013$Gender = as.double(noshows_20171013$Gender=="M")
noshows_20171013[1:13] <- lapply(noshows_20171013[1:13], as.numeric)
noshows_20171013 <- as.data.frame(noshows_20171013)
## association rules - noshow dataset
for (i in 1:13){noshows_20171013[,i]<-discretize(noshows_20171013[,i])}
rules_noshow <- apriori(noshows_20171013, parameter = list(supp = 0.25, conf = 0.95, target = "rules"));
plot(sort(rules_noshow, by="lift"))

## clustering

## crabs dataset

## elbow test
crabs_dataset <- read.csv("crabs.csv")
crabs_dataset[1:8] <- lapply(crabs_dataset[1:8], as.numeric)
ls.str(crabs_dataset)
crabs_dataset$sex <- NULL
crabs_dataset$sp <- NULL
df <- as.data.frame(crabs_data)
df$sex <- NULL
k.max <- 15
wss <- sapply(1:k.max, function(k) {kmeans(df, k, nstart=500,iter.max = 150 )$tot.withinss})
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="K - values",
     ylab="sum of squares")

## silhouette test (0-7)
kmm_crabs <- kmeans(df, 7, nstart = 20)
library(cluster)
library(HSAUR)
diSSE <- daisy(df)
dE2 <- diSSE^2
sk2 <- silhouette(kmm_crabs$cluster, dE2)
plot(sk2)

## dunn index test
result <- kmeans(df, 1, nstart = 20)
result1 <- kmeans(df, 2, nstart = 20)
result2 <- kmeans(df, 3, nstart = 20)
result3 <- kmeans(df, 4, nstart = 20)
result4 <- kmeans(df, 5, nstart = 20)
result5 <- kmeans(df, 6, nstart = 20)
result6 <- kmeans(df, 7, nstart = 20)
dun_pp=clv.Dunn(cls.scatt.data(df, result$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp1=clv.Dunn(cls.scatt.data(df, result1$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp2=clv.Dunn(cls.scatt.data(df, result2$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp3=clv.Dunn(cls.scatt.data(df, result3$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp4=clv.Dunn(cls.scatt.data(df, result4$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp5=clv.Dunn(cls.scatt.data(df, result5$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp6=clv.Dunn(cls.scatt.data(df, result6$cluster, dist="euclidean"), c("average"), c("aveToCent"))

## no shows clustering
library(readr)
setwd("/Users/vascofernandes/Desktop/projSAD")
noshows_20171013 <- read_delim("noshows.20171013.csv",";", escape_double = FALSE, trim_ws = TRUE)
install.packages('e1071', dependencies=TRUE)
require(caret)
noshows_20171013$PatientId <- NULL
noshows_20171013$AppointmentID <- NULL
noshows_20171013$ScheduledDay <- NULL
noshows_20171013$AppointmentDay <- NULL
noshows_20171013$Neighbourhood <- NULL
noshows_20171013$`No-show` <- NULL
## PCA
noshows_20171013$Gender = as.double(noshows_20171013$Gender=="M")
trans <- preProcess(noshows_20171013,method=c("center", "scale", "pca"))
PC = predict(trans, noshows_20171013)

## Sem PCA
noshow1 <- kmeans(noshows_20171013, 1)
noshow2 <- kmeans(noshows_20171013, 2)
noshow3 <- kmeans(noshows_20171013, 3)
noshow4 <- kmeans(noshows_20171013, 4)
noshow5 <- kmeans(noshows_20171013, 5)
noshow6 <- kmeans(noshows_20171013, 6)
noshow7 <- kmeans(noshows_20171013, 7)
library(clv)
dun_pp=clv.Dunn(cls.scatt.data(noshows_20171013, noshow1$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp1=clv.Dunn(cls.scatt.data(noshows_20171013, noshow2$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp2=clv.Dunn(cls.scatt.data(noshows_20171013, noshow3$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp3=clv.Dunn(cls.scatt.data(noshows_20171013, noshow4$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp4=clv.Dunn(cls.scatt.data(noshows_20171013, noshow5$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp5=clv.Dunn(cls.scatt.data(noshows_20171013, noshow6$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp6=clv.Dunn(cls.scatt.data(noshows_20171013, noshow7$cluster, dist="euclidean"), c("average"), c("aveToCent"))

## Com PCA
noshow_pc1 <- kmeans(PC, 1)
noshow_pc2 <- kmeans(PC, 2)
noshow_pc3 <- kmeans(PC, 3)
noshow_pc4 <- kmeans(PC, 4)
noshow_pc5 <- kmeans(PC, 5)
noshow_pc6 <- kmeans(PC, 6)
noshow_pc7 <- kmeans(PC, 7)

dun_pp.pca1 = clv.Dunn(cls.scatt.data(PC, noshow_pc1$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp.pca2 = clv.Dunn(cls.scatt.data(PC, noshow_pc2$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp.pca3 = clv.Dunn(cls.scatt.data(PC, noshow_pc3$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp.pca4 = clv.Dunn(cls.scatt.data(PC, noshow_pc4$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp.pca5 = clv.Dunn(cls.scatt.data(PC, noshow_pc5$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp.pca6 = clv.Dunn(cls.scatt.data(PC, noshow_pc6$cluster, dist="euclidean"), c("average"), c("aveToCent"))
dun_pp.pca7 = clv.Dunn(cls.scatt.data(PC, noshow_pc7$cluster, dist="euclidean"), c("average"), c("aveToCent"))


