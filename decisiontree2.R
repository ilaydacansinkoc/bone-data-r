library(rpart)
library(rpart.plot)
library(dplyr)

bone = read.table("spnbmd.csv", sep =",",header=T)
.data <- c("training", "test") %>%
       sample(nrow(bone), replace = T) %>%
       split(bone, .)
	   
treeAnalysis <- rpart(ethnic ~ spnbmd + age + sex, .data$training,model = "TRUE")
rpart.plot(treeAnalysis, extra = 4)