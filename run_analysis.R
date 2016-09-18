#************************************************************************************#

#Assignment: Getting and Cleaning Data Course Project
#Author: Sachin S; 
#Date: 09-17-2016

# Description:
# Dataset location: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#************************************************************************************#
library(dplyr)


#set working directory to the location where the UCI HAR Dataset was unzipped
path<-"C:\\Course3\\Week4\\Assignment\\UCIdataset\\UCI HAR Dataset"
setwd(path);

# Read feature & activity data 
activitylabels = read.table("./activity_labels.txt",header=FALSE); #imports activity_labels.txt
features     = read.table("./features.txt",header=FALSE); #imports features.txt


# Read training data 
subjecttrain = read.table("./train/subject_train.txt",header=FALSE); #imports subject_train.txt
xtrain       = read.table("./train/x_train.txt",header=FALSE); #imports x_train.txt
ytrain       = read.table("./train/y_train.txt",header=FALSE); #imports y_train.txt

#Read test data
subjecttest = read.table("./test/subject_test.txt",header=FALSE); #imports subject_test.txt
xtest       = read.table("./test/x_test.txt",header=FALSE); #imports x_test.txt
ytest       = read.table("./test/y_test.txt",header=FALSE); #imports y_test.txttd


#Provide descriptive names to variables
features[,2] <- gsub("-meanFreq()", ".mean.frequency", features[,2]) # substitutes "-meanFreq()" with ".mean.frequency"
features[,2] <- gsub("-mean()", ".mean", features[,2]) # substitutes "-mean" with ".mean"
features[,2] <- gsub("-std()", ".stddeviation", features[,2]) # substitutes "-std" with ".stddeviation"
features[,2] <- gsub("[-]", ".", features[,2]) 
features[,2] <- gsub("[()]", "", features[,2]) 

#select mean & stand deviation training measurements
tdf_xtrain<-tbl_df(xtrain)
tdf_xtrain<-select(tdf_xtrain,c(grep("mean|stddeviation", features[,2], value = FALSE)))

#select mean & stand deviation test measurements
tdf_xtest<-tbl_df(xtest)
tdf_xtest<-select(tdf_xtest,c(grep("mean|stddeviation", features[,2], value = FALSE)))

#Set columnnames of the training data
colnames(activitylabels)  <- c("activityid","activitytype");
colnames(subjecttrain)  <- "subjectid"
colnames(tdf_xtrain)<-c(grep("mean|stddeviation", features[,2], value = TRUE))
colnames(ytrain) <-"activityid";

#Set column names to the test data 
colnames(subjecttest) = "subjectid";
colnames(tdf_xtest)<-c(grep("mean|stddeviation", features[,2], value = TRUE))
colnames(ytest)       = "activityid";

#Create the final test set by merging the xtest, ytest and subjecttest data
trainingdata <- cbind(ytrain,subjecttrain,tdf_xtrain)

# Create the final test set by merging the xtest, ytest and subjecttest data
testdata = cbind(ytest,subjecttest,tdf_xtest)

# merge training & test data
mdata <- rbind(trainingdata,testdata)

#add activity label to the final data set
finaldata<-inner_join(activitylabels,mdata,by="activityid")
finaldata<-select(finaldata,-activityid)

#calculate mean of all variables by activitytype & subjectid
grp<-group_by(finaldata,activitytype,subjectid)
tidydata<-summarise_each(grp,funs(mean))

#writed tidydata to a file
write.table(tidydata, "./tidydata.txt",row.names=FALSE,sep="\t")