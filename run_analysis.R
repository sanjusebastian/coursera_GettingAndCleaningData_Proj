
##########################################################################################################

### Coursera Getting and Cleaning Data Course Project
### Sanju Sebastian
### 08-05-2017

# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################


# Clean up workspace
rm(list=ls())

# SECTION 1. Merge the training and the test sets to create one data set.

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd('/home/sanju/UCI HAR Dataset');

# Read in the data from files
featurelist     = read.table('./features.txt',header=FALSE); #imports features.txt
activitytypesdf = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt. labels/description for activityids
subjecttraindf = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt. Contains subjectids
xtraindf       = read.table('./train/X_train.txt',header=FALSE); #imports x_train.txt. x_train.txt contains all the meaasurements.
ytraindf       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt which the activity id ies corresponding to the activity measurements in x_train.txt

# Assigin column names to the data imported above
colnames(activitytypesdf )  = c('activityid','activitytype');
colnames(subjecttraindf)  = "subjectid";
colnames(xtraindf)        = featurelist[,2]; 
colnames(ytraindf)        = "activityid";

# create the training set by merging ytraindf,subjecttraindf,xtraindf using cbind. 
#Assumption is that rows are in order in each file based on the no of rows in each file. This should have explicity mentioned in the assignment. 
#I come from a database background where merging and creating files without a common key is a strict no no. 

traindf= cbind(ytraindf,subjecttraindf,xtraindf);

#traindf now contains activityid,subjectid and all the features

# reading data from test dataset.follow same steps as traindf, before doing the final merge. except the features
subjecttestdf = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xtestdf       = read.table('./test/X_test.txt',header=FALSE); #imports x_test.txt
ytestdf       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Repeating the steps on the on the test data
colnames(subjecttestdf) = "subjectid";
colnames(xtestdf)= featurelist[,2]; 
colnames(ytestdf)= "activityid";


# Create testdf by column bind similar to traindf
testdf = cbind(ytestdf,subjecttestdf,xtestdf);


# Combine test and train with rbind to create  final df
finaldf = rbind(traindf,testdf);

# End of SECTION 1. Merge the training and the test sets to create one data set.

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(finaldf); 

# SECTION 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others.
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finaldf = finaldf[logicalVector==TRUE];

# END of SECTION 2. Extract only the measurements on the mean and standard deviation for each measurement. 

#SECTION 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finaldf = merge(finaldf ,activitytypesdf,by='activityid',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finaldf); 

# END of SECTION 3. Use descriptive activity names to name the activities in the data set

# SECTION 4. Appropriately label the data set with descriptive activity names for tidydata.

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccelMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccelJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finaldf) = colNames;

# END of SECTION 4. Appropriately label the data set with descriptive activity names for tidydata.

# SECTION 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finaldfNoActivityType  = finaldf[,names(finaldf) != 'activitytype'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finaldfNoActivityType[,names(finaldfNoActivityType) != c('activityid','subjectid')],by=list(activityid=finaldfNoActivityType$activityid,subjectid = finaldfNoActivityType$subjectid),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activitytypesdf,by='activityid',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t');
# END of SECTION 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 