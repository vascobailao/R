source("lab2.r", echo = TRUE)

setwd("/Users/vascofernandes/Desktop/projSAD")

## 1 a)
ex_tr=read.transactions("example.csv",format="basket", sep=",")
inspect(ex_tr)
is <- apriori(ex_tr, parameter = list(support= 0.2, target="frequent"))
inspect(is)

## 1 b)
adult=read.transactions("adult.csv", format="basket", sep=",")
rules <- apriori(adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules")) 
inspect(rules)
quality(rules)

## 2

vote <- read.transactions("vote.csv", format = "basket", sep = ",")
rules <- apriori(vote, parameter = list(supp = 0.1, conf = 1, target = "rules"))
inspect(rules)

## a)
rules <- apriori(vote, parameter = list(supp = 0.1, conf = 0.9, target = "rules")) ## 24 rules
inspect(rules)

rules <- apriori(vote, parameter = list(supp = 0.2, conf = 1, target = "rules"))
inspect(rules)

rules <- apriori(vote, parameter = list(supp = 0.2, conf = 0.9, target = "rules"))
inspect(rules)

rules <- apriori(vote, parameter = list(supp = 0.2, conf = 0.8, target = "rules"))
inspect(rules)




