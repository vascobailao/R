setwd("/Users/vascofernandes/Desktop/projSAD")

crabs <- read.csv("crabs.csv")
pairs(crabs[,4:8])

crabs.f = crabs
crabs.f$sp <- NULL
crabs.f$sex <- NULL
crabs.f <- scale(crabs.f[-1])
results <- kmeans(crabs.f, 3)

table(crabs$sex, results$cluster)


library(ggplot)
ggplot(crabs.f, aes(x=crabs$FL, y=crabs$RW, color = sp)) + geom_point()