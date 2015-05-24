library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(e1071)
library(randomForest)
library(rpart)







library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
df <- SAheart
modFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA, method="glm", family="binomial")
pred <- predict(modFit, testSA)
testSA$predRight <- pred==testSA$chd
tab <- table(pred,testSA$chd)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
pred <- predict(modFit, trainSA)
missClass(trainSA$chd,pred)
pred <- predict(modFit, testSA)
missClass(testSA$chd,pred)



df <- olive
set.seed(125)
index <- createDataPartition(df$Area, p = 0.8, times = 1, list = FALSE)
training <- df[index,]
testing <- df[-index,]


modFit <- train(Area ~ ., data = training, method="rpart")
pred <- predict(modFit, testing)
testing$predRight <- pred==testing$Area
table(pred,testing$Area)

acc <- pred==testing$Class
summary(acc)
fancyRpartPlot(modFit$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
newdata
pred <- predict(modFit, newdata)
pred



df <- segmentationOriginal
set.seed(125)
index <- createDataPartition(df$Case, p = 0.5, times = 1, list = FALSE)
training <- df[index,]
testing <- df[-index,]


modFit <- train(Class ~ ., data = training, method="rpart")
pred <- predict(modFit, testing)
testing$predRight <- pred==testing$Class
table(pred,testing$Class)

acc <- pred==testing$Class
summary(acc)
fancyRpartPlot(modFit$finalModel)




cf <- df[1,]

cf$TotalIntenCh2[1]=23000
cf$FiberWidthCh1[1]=10
cf$PerimStatusCh1[1]=2
pred <- predict(modFit, cf)
pred

cf <- df[1,]
cf$TotalIntenCh2[1]=50000
cf$FiberWidthCh1[1]=10
cf$VarIntenCh4[1]=100
pred <- predict(modFit, cf)
pred

cee <- data.frame(TotalIntench2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1=2 )
