library("loon")
library("dplyr")
library("ggplot2")

bone = read.table("spnbmd.csv", sep =",",header=T)

ethnic <- bone$ethnic
age <- bone$age
sex <- bone$sex
spnbmd <- bone$spnbmd

bone <- bone %>% group_by(ethnic) %>% mutate(n = n()/nrow(bone))

ggplot(bone, aes(x=ethnic, y=spnbmd, fill= ethnic)) + geom_violin(aes(weight = n), col = NA)