library("loon")
library("dplyr")
library("ggplot2")
bone = read.table("spnbmd.csv", sep =",",header=T)

ethnic <- bone$ethnic
age <- bone$age
sex <- bone$sex
spnbmd <- bone$spnbmd

# add weights
ggplot(bone, aes(x=spnbmd, fill= ethnic)) + geom_density(col = NA,alpha=0.35)


