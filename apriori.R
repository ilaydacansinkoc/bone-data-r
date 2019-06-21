library(arules)
library(arulesViz)
library(colorspace)

bone = read.delim(file = "bone.data")
bone[1] <- NULL
bone = as(bone, 'data.frame')

head(bone)
summary(bone)

rules <- apriori(bone, parameter = list(support = 0.009, confidence = 0.25, minlen = 2))
rules <- subset(rules, subset = rhs %pin% "spnbmd=")

summary(rules)

plot(rules,method="matrix", measure="lift")

inspect(rules)