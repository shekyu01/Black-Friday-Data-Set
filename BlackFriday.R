### Code for Black Friday Data Hack on AnalyticsVidhya
### Author: Chetan Shekdar


            ###########################################
            #######Exploratory Data Analysis###########
            ###########################################


#working directory
path <- "C:/Users/yudhisthirs/Desktop/DataCamp_Material/10.Projects/4. Black Friday"

#set working directory
setwd(path)

#loading train and test
train <- read.csv("train.csv",stringsAsFactors=FALSE)
test <- read.csv("test.csv",stringsAsFactors=FALSE)


#checking dimension 
dim(train)
dim(test)

#structure of train 
str(train)


#check if this data has missing values
colSums(is.na(train))
colSums(is.na(test))

#summary 
summary(train)
summary(test)


            
            ###########################################
            ##Graphical Representation of Variables####
            ###########################################




library(ggplot2)
str(train)

#Gender vs Purchase
ggplot(train, aes(Gender, Purchase)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + ggtitle("Gender vs Purchase") + theme_bw()
#it seems that male are contributing more than female when its comes to Sale

#Age vs Purchase
ggplot(train, aes(Age, Purchase)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + ggtitle("Age vs Purchase") + theme_bw()
# Age group 26-35 is contributing more followed by 36-45 and 18-25

#Occupation vs Purchase
ggplot(train, aes(Occupation, Purchase)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + ggtitle("Occupation vs Purchase") + theme_bw()
#The professional who are following 0,4,7 job code are contributing more in sell.

#City_Category vs Purchase
ggplot(train, aes(City_Category, Purchase)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + ggtitle("City_Category vs Purchase") + theme_bw()
#out of A,B,C , ppl those are belongs to city catogory B are frq purchasing habit 


#Stay_In_Current_City_Years vs Purchase
ggplot(train, aes(Stay_In_Current_City_Years, Purchase)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + ggtitle("Stay_In_Current_City_Years vs Purchase") + theme_bw()
#ppl who are there in city for 1 year contributing more sells

#Stay_In_Current_City_Years vs Purchase
ggplot(train, aes(Product_Category_1, Purchase)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + ggtitle("Product_Category_1 vs Purchase") + theme_bw()
#In Cate2 and 3 we do have lots of missing values.Lets deal with missing values






              ###########################################
              ########Dealing with missing obs###########
              ###########################################
     

colSums(is.na(train))
colSums(is.na(test))
#In Product_Category_2 & Product_Category_3 we do hv 245982 & 545809 respectivly

#Replacing Product_Category_2  missing values with Mean
summary(train$Product_Category_2)
train$Product_Category_2[is.na(train$Product_Category_2)] =median(train$Product_Category_2, na.rm=TRUE)
test$Product_Category_2[is.na(test$Product_Category_2)] =median(test$Product_Category_2, na.rm=TRUE)


#Replacing Product_Category_3  missing values with Mean
summary(train$Product_Category_3)
train$Product_Category_3[is.na(train$Product_Category_3)] =median(train$Product_Category_3, na.rm=TRUE)
test$Product_Category_3[is.na(test$Product_Category_3)] =median(test$Product_Category_3, na.rm=TRUE)



              ###########################################
              ########  Dealing with outliers ###########
              ###########################################




#running for outlier 
hist(combi$Product_ID, breaks = 10)

boxplot(train$Product_Category_1, horizontal = TRUE)
boxplot(test$Product_Category_1, horizontal = TRUE)
#some outliers are present in Product_Category_1
X_train <- subset(train, !Product_Category_1 %in% c(19,20))
X_test <- test


boxplot(combi$Product_Category_2, horizontal = TRUE)
#no outliers


boxplot(combi$Product_Category_3, horizontal = TRUE)
#no outliers


              
              ###########################################
              #########Feature Engineering#################
              ###########################################


library(dummies)


# onehot-encoding city variable
X_train <- dummy.data.frame(train, names=c("City_Category"), sep="_")
X_test <- dummy.data.frame(X_test, names=c("City_Category"), sep="_")


# converting age variable to numeric
X_train$Age[X_train$Age == "0-17"] <- "15"
X_train$Age[X_train$Age == "18-25"] <- "21"
X_train$Age[X_train$Age == "26-35"] <- "30"
X_train$Age[X_train$Age == "36-45"] <- "40"
X_train$Age[X_train$Age == "46-50"] <- "48"
X_train$Age[X_train$Age == "51-55"] <- "53"
X_train$Age[X_train$Age == "55+"] <- "60"

X_test$Age[X_test$Age == "0-17"] <- "15"
X_test$Age[X_test$Age == "18-25"] <- "21"
X_test$Age[X_test$Age == "26-35"] <- "30"
X_test$Age[X_test$Age == "36-45"] <- "40"
X_test$Age[X_test$Age == "46-50"] <- "48"
X_test$Age[X_test$Age == "51-55"] <- "53"
X_test$Age[X_test$Age == "55+"] <- "60"

X_train$Age <- as.integer(X_train$Age)
X_test$Age <- as.integer(X_test$Age)


# converting stay in current city to numeric
X_train$Stay_In_Current_City_Years[X_train$Stay_In_Current_City_Years == "4+"] <- "4"
X_test$Stay_In_Current_City_Years[X_test$Stay_In_Current_City_Years == "4+"] <- "4"

X_train$Stay_In_Current_City_Years <- as.integer(X_train$Stay_In_Current_City_Years)
X_test$Stay_In_Current_City_Years <- as.integer(X_test$Stay_In_Current_City_Years)

# converting gender to binary
X_train$Gender <- ifelse(X_train$Gender == "F", 1, 0)
X_test$Gender <- ifelse(X_test$Gender == "F", 1, 0)


# feature representing the count of each user
library(plyr)

user_count <- ddply(X_train, .(User_ID), nrow)
names(user_count)[2] <- "User_Count"
X_train <- merge(X_train, user_count, by="User_ID")
X_test <- merge(X_test, user_count, all.x=T, by="User_ID")


# feature representing the count of each product
product_count <- ddply(X_train, .(Product_ID), nrow)
names(product_count)[2] <- "Product_Count"
X_train <- merge(X_train, product_count, by="Product_ID")
X_test <- merge(X_test, product_count, all.x=T, by="Product_ID")
X_test$Product_Count[is.na(X_test$Product_Count)] <- 0

# feature representing the average Purchase of each product
product_mean <- ddply(X_train, .(Product_ID), summarize, Product_Mean=mean(Purchase))
X_train <- merge(X_train, product_mean, by="Product_ID")
X_test <- merge(X_test, product_mean, all.x=T, by="Product_ID")
X_test$Product_Mean[is.na(X_test$Product_Mean)] <- mean(X_train$Purchase)


              
              ###########################################
              #######Multiple Linear Regression model####
              ###########################################



#Let's now build out first regression model on this data set.
linear_model <- lm(Purchase ~ ., data = X_train)
summary(linear_model)

library(Metrics)
rmse(X_train$Purchase, linear_model$fitted.value)




              
              
              ###########################################
              #######  Decision Trees model##############
              ###########################################


#loading required libraries
library(rpart)
library(e1071)
library(rpart.plot)
library(caret)

#setting the tree control parameters
fitControl <- trainControl(method = "cv", number = 5)
cartGrid <- expand.grid(.cp=(1:50)*0.01)

#decision tree
tree_model <- train(Purchase ~ ., data = X_train, method = "rpart",trControl = fitControl, tuneGrid = cartGrid)
print(tree_model)


#The final value for cp = 0.01. You can also check the table populated in console for more information. The model
#with cp = 0.01 has the least RMSE. Let's now build a decision tree with 0.01 as complexity parameter.


main_tree <- rpart(Purchase ~ ., data = X_train, control =rpart.control(cp=0.01))
prp(main_tree)


pre_score <- predict(main_tree, type = "vector")
rmse(X_train$Purchase, linear_model$fitted.value)



              
              ###########################################
              #######  Random Forest       ##############
              ###########################################
              



#load randomForest library
library(randomForest)

#set tuning parameters
control <- trainControl(method = "cv", number = 5)

#random forest model
rf_model <- train(Purchase ~ ., data = X_train, method = "parRF", trControl = control, prox = TRUE, allowParallel = TRUE)
print(rf_model)


#Now we've got the optimal value of mtry = 15. Let's use 1000 trees for computation.
#random forest model
forest_model <- randomForest(Purchase ~ ., data = X_train, mtry = 15,ntree = 1000)
print(forest_model)
varImpPlot(forest_model)


#We can further tune the parameters and Build model using XGBoost to get less RMSE




