train<-read.csv(".\\train.csv")
test<- read.csv(".\\test.csv")


str(train)
str(test)
summary(train)

hist(train$label)

train2<-train

train2$label<- as.factor(train$label)

pixel.columns <- paste0("pixel", 0:783)
fmla <- as.formula(paste("train2$label ~ ", paste(pixel.columns, collapse= "+")))

model<- glm(fmla,train2, family="binomial")

prediction <- round(predict.glm(model,test))

write.csv(data.frame(ImageId=1:nrow(test),label=prediction) , file = "basic_glm.csv", row.names = FALSE)
