## BASIC GENDER BASED MODEL

test<-read.csv(".\\test.csv")
train<-read.csv(".\\train.csv")

str(train)
str(test_1)

test$Survived<-0
test$Survived[test$Sex=="female"]<-1

test_1<- as.data.frame(cbind(test$PassengerId,test$Survived)) 
colnames(test_1)<- c("PassengerId","Survived")
write.csv(test_1,file="test_1.csv",row.names = FALSE)

test_02<-read.csv(".\\test_1.csv")



## BASIC DECISION TREE MODEL
# Your train and test set are still loaded in
str(train)
str(test)

library(rpart)

# Build the decision tree
my_tree <- rpart(Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare + Embarked, data=train, method="class")

# Visualize the decision tree using plot() and text()
plot(my_tree)
text(my_tree)

# Load in the packages to create a fancified version of your tree
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")

library(rattle)

library(rpart.plot)
library(RColorBrewer)

# Time to plot your fancified tree
fancyRpartPlot(my_tree)




## Making a prediction using Decision Trees
# Your train and test set are still loaded in
str(train)
str(test)

# Make your prediction using the test set
my_prediction <- predict(my_tree,test,type="class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Check that your data frame has 418 entries
str(my_solution)

# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution, file="decision_tree.csv" , row.names = FALSE)


#TWEKING TREE PARAMETERS

my_tree_3 <- rpart(Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare + Embarked, data=train, method="class",minsplit=50,cp=0)
fancyRpartPlot(my_tree_3)
fancyRpartPlot(my_tree)


#FEATURE ENGINEERING
train_2<-train
train_2$Family_Size<-train_2$SibSp+train_2$Parch
my_tree_4 <- rpart(Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare + Embarked + Family_Size, data=train_2, method="class",minsplit=50,cp=0)
fancyRpartPlot(my_tree_4)





#DEALING WITH MISSING VALUES

# All data, both training and test set
all_data <-rbind(train,test)

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
# We code all embarkment codes as factors.
all_data$Embarked[c(62,830)] = "S"
all_data$Embarked <- factor(combi$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data=all_data[!is.na(all_data$Age),], method="anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Split the data back into a train set and a test set
train <- all_data[1:891,]
test <- all_data[892:1309,]




#######VVIP ######
#### RANDOM FOREST EXAMPLE######

# train and test are available in the workspace
str(train)
str(test)

# Load in the package
install.packages("randomForest")
library(randomForest)

# Train set and test set
str(train)
str(test)

# Set seed for reproducibility
set.seed(111)

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Cabin, 
                          data=train, importance=TRUE, ntree=1000)

# Make your prediction using the test set
my_prediction <- predict(my_forest, test)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)







##VISUALIZING RANDOM FOREST VARIABLE IMPORTANCE####
varImpPlot(my_forest)








