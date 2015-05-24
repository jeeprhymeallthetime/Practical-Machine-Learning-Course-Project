setwd("~/GitHub/Practical-Machine-Learning-Course-Project")
df <- read.csv("pml-training.csv", header = true)
index <- createDataPartition(df$classe,p=0.8,list=FALSE)
training <- df[index,]
testing <- df[-index,]
