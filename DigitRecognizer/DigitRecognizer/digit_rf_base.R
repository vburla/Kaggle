train<-read.csv(".\\train.csv")
test<- read.csv(".\\test.csv")

numTrain <- 10000
numTrees <- 25

rows <- sample(1:nrow(train), numTrain)
labels <- as.factor(train[,1])
train <- train[,-1]

rf <- randomForest(train, labels, xtest=test, ntree=100)
predictions <- data.frame(ImageId=1:nrow(test), Label=levels(labels)[rf$test$predicted])
head(predictions)

write.csv(predictions, "rf_benchmark.csv",row.names = FALSE) 
