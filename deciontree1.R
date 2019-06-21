library(rpart)
library(rpart.plot)
library(dplyr)

bone = read.delim(file = "bone.data")
bone[1] <- NULL
.data <- c("training", "test") %>%
       sample(nrow(bone), replace = T) %>%
       split(bone, .)

treeAnalysis <- rpart(bone$gender~bone$spnbmd+bone$age,.data$training,model = "TRUE")
rpart.plot(treeAnalysis, extra = 4)

