#Seattle Analysis
#Kalindi Deshmukh


#import and explore data
rev_data<- read_excel("Capgemini/rev data for test.xlsx")
View(rev_data)
dim(rev_data)
summary(rev_data)
#converting Type to categorical variable
rev_data$Type<-as.factor(rev_data$Type)
#checking for missing values
View(colSums(is.na(rev_data)))
#####################################################################################################################
#1.A: What is the most common reason for calling 911?

table(rev_data$Type)
#Beaver Accident            Latte Spills Marshawn Lynch Sighting             Seal Attack 
#508                     416                     324                     266 

#####################################################################################################################
#1.B: Display these results graphically

barplot(table(rev_data$Type), ylim=c(0,550),  axes=F, col= c("lavender", "darkblue"), 
         main="Reasons for calling 911 (Seattle)")
axis(side = 2, at = seq(from=0, to=550, by=100))
box(lwd=2.5)

#####################################################################################################################

#2.A: Please create a graph of the 911 calls using the 'Latitude' and 'Longitude' (graph type is up to you)
#(differentiate call type using colors)


library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)           

#Get the latest Install
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE) #Enter the option number with 'All'

#Load the library
library("ggmap")

#Set your API Key
ggmap::register_google(key = "AIzaSyAam9bEmtnqpO2qeApqzKbAYmcdBTf3iwY") #add other key if you have

#Notes: If you get still have a failure then I suggest to restart R and run the library and register 
#google commands again.


p <- ggmap(get_googlemap(center = c(lon = -122.335167, lat = 47.608013),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))

p + geom_point(aes(x = Longitude, y = Latitude,  colour = Type), data = rev_data, size = 1) + 
  theme(legend.position="bottom")

################################################################################################################


#2.B: Are there any data points that look mislabeled?

plot(rev_data$Longitude~rev_data$Latitude, 
     col = c("red", "green", "lightblue", "violet")[rev_data$Type],
     main="Scatterplot for Latitude and Longitude", 
     ylab="Longitude", 
     xlab="Lattitude",
     pch= 20)


###############################################################################################################

#3.A: If we were to use only 'Latitude' and 'Longitude', could we make an intelligent decision
#as to why a resident dialed 911? (In other words, if we take off the labels - can we still 
#determine which category a 911 call would most likely fall into?) Please describe this algorithm
#and your reason for choosing it.

#Take a subset of longitude and latitude only
#Remove other variables
rev2 <- rev_data[ -c(1, 4) ]

#calculate wss (within-cluster sum of squares)
wss <- sapply(1:10, 
              function(clusters){kmeans(rev2, clusters, nstart=100,iter.max = 10 )$tot.withinss})

#plot the graph for number of clusters
plot(1:10, wss,
     type="b", pch = 10, frame = FALSE, 
     main= "Scree Plot K means clustering",
     xlab="No of clusters",
     ylab="Total WSS") 

#looking for elbow using line
abline(h=2.5, col="steelblue", lwd=2.5) 

box(lwd=2.5)

#######KNN#######
library(class)
set.seed(462)

#Sample the data
indexes = sample(1:nrow(rev_data), size=0.3*nrow(rev_data),replace=TRUE)

# Split sampled data and take subset for latitude and longitutde

#training data
revdata_training = rev_data[-indexes,]
revdata_training2 <- revdata_training[-c(1, 4)]

#testing data
revdata_testing = rev_data[indexes,]
dim(revdata_testing) 
revdata_testing2 <- revdata_testing[-c(1, 4)]

# Storing the labels
train_labels  <- revdata_training$Type
test_labels  <- revdata_testing$Type

# Train the model with KNN algorithm
rev_test_pred <- knn(train = revdata_training2, test = revdata_testing2, cl= train_labels,k = 4,prob=TRUE)

#Evaluate the model performance
library(gmodels)
CrossTable(x = test_labels, y = rev_test_pred,prop.chisq=FALSE) 
##########################################################################################################

#3.C: Please display the results of your algorithm, along with the associated code
#3.D: Please display the number of correct categorizations

library(class)
require("class")
#Remove Report Location as it is character
rev_data$`Report Location`<-NULL

set.seed(462)

#Sample the data
indexes = sample(1:nrow(rev_data), size=0.3*nrow(rev_data),replace=TRUE)

#Split data into training and validation sets
revdata_validation2<-rev_data[indexes,]
dim(revdata_validation2)  
summary(revdata_validation2)
revdata_training3<-rev_data[-indexes,]
dim(revdata_training3)
summary(revdata_training3)

# Create a Random Forest model with default parameters
install.packages("randomForest")
library(randomForest)
model1 <- randomForest(Type ~ ., data = revdata_training3)
model1
plot(model1)
summary(model1)

# Fine tuning parameters of Random Forest model
model2 <- randomForest(Type ~ ., data = revdata_training3, ntree = 500,importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, revdata_training3, type = "class")
# Checking classification accuracy
table(predTrain, revdata_training3$Type)  

# Predicting on Validation set
predValid <- predict(model2, revdata_validation2, type = "class")
# Checking classification accuracy
mean(predValid == revdata_validation2$Type)                  
table(predValid,revdata_validation2$Type) #####Correct Categorisations

# To check important variables
importance(model2)        
varImpPlot(model2) 

# Compare with Decision Tree

install.packages("rpart")
install.packages("caret")
install.packages("e1071")

library(rpart)
library(caret)
library(e1071)

# We will compare model 1 of Random Forest with Decision Tree model

model_dt = train(Type ~ ., data = revdata_training3, method = "rpart")
model_dt_1 = predict(model_dt, data = revdata_training3)
table(model_dt_1, revdata_training3$Type)

mean(model_dt_1 == revdata_training3$Type)

# Running on Validation Set
model_dt_vs = predict(model_dt, newdata = revdata_validation2)
table(model_dt_vs, revdata_validation2$Type)

mean(model_dt_vs == revdata_validation2$Type)

###########################################################################################################



