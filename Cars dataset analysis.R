
#Importing the file
                           
library(readr)
mydf <- read_delim("auto_mpg.csv", ";", 
                       escape_double = FALSE, trim_ws = TRUE) #telling R to read CSV files for file upload 
View(mydf) # importing CSV file into R environment and naming it "mydf"


#How many missing values does the mydf have
sum(is.na(mydf)) # 14 missing values



#Deleting rows with missing values
install.packages("tidyr")   # downloading a package needed to call the drop_na function
library(tidyr)              # calling a package to use drop_na function

mydf<-drop_na(mydf)         # saving a dataset with removed missing values
sum(is.na(mydf))            # 0 missing values



#Deleting a select column from the dataset
install.packages("dplyr")        # downloading a package needed to call the function to remove select columns
library(dplyr)                   # calling a package to use the function to drop select columns

mydf = select(mydf, -orogin)     # removing the "orogin" column from mydf


### Business Questions ###

#What is the 75th percentile of mpg metric for available cars?
summary(mydf$mpg)   # The 75th percentile of cars by mpg are between 29 and 46



# What are the car names that match the 75th percentile of cars by mpg?
good_cars<-subset(mydf, mydf$mpg>=29)  # Creating a new data frame with cars that have mpg of 29 or higher 
good_cars                              # Viewing the new data frame to see the names of these cars



# What cars within the 75th percentile of cars by mpg are built in 1975 or later?
new_cars<-subset(good_cars, good_cars$model_year>=75)   # Creating a new object to see what cars are made in 1975 or lower
new_cars                                                # Viewing the new data frame to see the names of these cars



# How did the vehicle fuel consumption improve between 1970 and 1982 by 4-year increments?

# Using a for loop to create 3 groups by model_year (70-73, 74-77, 78-81)
for(i in 1:nrow(mydf)){
  if(mydf$model_year[i]>=70 & mydf$model_year[i]<=73){mydf$model_year_group[i]<-'group #1'}
  else if(mydf$model_year[i]>=74 & mydf$model_year[i]<=77){mydf$model_year_group[i]<-'group #2'}
  else if(mydf$model_year[i]>=78 & mydf$model_year[i]<=81){mydf$model_year_group[i]<- 'group #3'}
  else if(mydf$model_year[i]==82){mydf$model_year_group[i]<- 'group #4'}
}
View(mydf)          # Viewing mydf to ensure the new column with groups was created properly

library(ggplot2)    # Importing ggplot2 package to visualize the data

group_mpg_plot<-ggplot(mydf, aes(mydf$model_year_group, mydf$mpg)) #plotting the relationship between group number and mpg
group_mpg_plot + geom_point(shape=3)
group_mpg_plot + geom_point(aes(colour=model_year_group)) #adding color to separate groups

summary(mydf[1:124, 1])    #mpg summary of group #1  || group #1 mean is 18.5
summary(mydf[125:242, 1])  #mpg summary of group #2  || group #2 mean is 22 (19% increase)
summary(mydf[243:362, 1])  #mpg summary of group #3  || group #3 mean is 28 (27% increase)
summary(mydf[363:392,1])   #mpg summary of group #4  || group #4 mean is 32

####################################################################???
#Now we want to find out about the fuel consumption <30MPG is success

#Build a logistic regression for mpg
#Create a column "good_bad" that represents success with 1 being success and 0 = failure

for(i in 1:nrow(mydf)){
  if (mydf$mpg[i]>=30){mydf$good_bad[i]<-"1"}
  else{mydf$good_bad[i]<-"0"}
}

#converting the good_bad column back to nummeric for further use
mydf$good_bad <- as.numeric(mydf$good_bad)

#evaluating the results of the good_bad column
table(mydf$good_bad)

#Make a logistic regression to see the related factors

mpg_influence<-glm(good_bad ~ cylinders+horsepower+weight+acceleration, data=mydf, family="binomial")
summary(mpg_influence)

(exp(-0.0572518)-1)*100                          # with every increase of cylinders by 1 unit, the probability of success decrease by 5.56%
(exp(-0.1080746)-1)*100                          # with every increase of horsepower by 1 unit, the probability of success decrease by 10.24%
(exp(-0.0009030)-1)*100                          # with every increase of weight by 1 unit, the probability of success decrease by 0.09%
(exp(-0.2676784)-1)*100                          # with every increase of acceleration by 1 unit, the probability of success decrease by 23.48%

library(rpart)
library(rpart.plot)
tree_mydf<-rpart(good_bad~cylinders+horsepower+weight+acceleration, data=mydf, method="class")
rpart.plot(tree_mydf, extra=1, type=1)   

tree_pred<-predict(tree_mydf, mydf, type="prob")              #prediction model for tree
logit_pred<-predict(mpg_influence, mydf, type="response")     #prediction model for the logistic regression

library(ROCR)
pred_val_tree<-prediction(tree_pred[,2], mydf$good_bad)
pred_val_logit<-prediction(logit_pred, mydf$good_bad)

perf_tree<-performance(pred_val_tree, "tpr", "fpr")
perf_logit<-performance(pred_val_logit, "tpr", "fpr")
plot(perf_tree, col="black")
plot(perf_logit, col='red', add=TRUE)






