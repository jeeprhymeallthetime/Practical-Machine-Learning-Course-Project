library("caret")
setwd("~/GitHub/Practical-Machine-Learning-Course-Project")
df <- read.csv("pml-training.csv", na.strings=c("#DIV/0!", "NA"))
datatesting <- read.csv("pml-testing.csv", na.strings=c("#DIV/0!", "NA"))

index <- createDataPartition(df$classe,p=0.8,list=FALSE)
training <- df[index,]
testing <- df[-index,]




###Make a bunch of models using all the parameters without NAs.  Compare their accuracy.  
###Then start combining predictors and compare their accuracies.  
###Then figure out if you can improve accuracy by removing predictors



rf1 <- randomForest(classe ~ gyros_forearm_x + gyros_forearm_y + gyros_forearm_z + accel_forearm_x + accel_forearm_y + accel_forearm_z + magnet_forearm_x + magnet_forearm_y + magnet_forearm_z, method="rf", data=training, importance=TRUE)
rf2 <- randomForest(classe ~  gyros_forearm_x + gyros_forearm_y + gyros_forearm_z + accel_forearm_x + accel_forearm_y + accel_forearm_z + magnet_forearm_x + magnet_forearm_y + magnet_forearm_z + num_window + roll_belt + pitch_belt + yaw_belt + total_accel_belt + gyros_belt_x + gyros_belt_y + gyros_belt_z + accel_belt_x + accel_belt_y + accel_belt_z + magnet_belt_x + magnet_belt_y + magnet_belt_z + roll_arm + pitch_arm + yaw_arm + total_accel_arm + gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + accel_arm_y + accel_arm_z + magnet_arm_x + magnet_arm_y + magnet_arm_z + roll_dumbbell + pitch_dumbbell + yaw_dumbbell + gyros_dumbbell_x + gyros_dumbbell_y + gyros_dumbbell_z + accel_dumbbell_x + total_accel_dumbbell + accel_dumbbell_y + accel_dumbbell_z + magnet_dumbbell_x + magnet_dumbbell_y + magnet_dumbbell_z + roll_forearm + pitch_forearm + yaw_forearm +total_accel_forearm, method="rf", data=training, importance=TRUE)


summary(predict(rf1,testing)==testing$classe)
summary(predict(rf2,testing)==testing$classe)
confusionMatrix(testing$classe, predict(rf2, testing))

##It feels as though this is over-fitting, so I'm going to re-run these models to find the most important variables

names(sort(importance(rf2)[,7], decreasing=TRUE))[1:10]
rf3 <- randomForest(classe ~  num_window+roll_belt+yaw_belt+pitch_forearm+magnet_dumbbell_z+pitch_belt+magnet_dumbbell_y, method="rf", data=training, importance=TRUE)
rf4 <- randomForest(classe ~  num_window+roll_belt+yaw_belt+pitch_forearm+magnet_dumbbell_z+pitch_belt+magnet_dumbbell_y+roll_forearm+magnet_dumbbell_x+roll_dumbbell, method="rf", data=training, importance=TRUE)
summary(predict(rf3,testing)==testing$classe)
summary(predict(rf4,testing)==testing$classe)
confusionMatrix(testing$classe, predict(rf4, testing))
rf5<- randomForest(classe ~  num_window+roll_belt+yaw_belt+pitch_forearm+magnet_dumbbell_z, method="rf", data=training, importance=TRUE)
rf6<- randomForest(classe ~  num_window, method="rf", data=training, importance=TRUE)
rf7 <- randomForest(classe ~ roll_belt+yaw_belt+pitch_forearm+magnet_dumbbell_z+pitch_belt+magnet_dumbbell_y+roll_forearm+magnet_dumbbell_x+roll_dumbbell, method="rf", data=training, importance=TRUE)
rf8<- randomForest(classe ~  roll_belt+yaw_belt+pitch_forearm+magnet_dumbbell_z, method="rf", data=training, importance=TRUE)
rf9<- randomForest(classe ~ yaw_belt, method="rf", data=training, importance=TRUE)
as.character(predict(rf1,datatesting))
as.character(predict(rf2,datatesting))
as.character(predict(rf3,datatesting))
as.character(predict(rf4,datatesting))
as.character(predict(rf5,datatesting))
as.character(predict(rf6,datatesting))
as.character(predict(rf7,datatesting))
as.character(predict(rf8,datatesting))
as.character(predict(rf9,datatesting))

####All models pass.