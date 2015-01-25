## Getting data - Project 2 ******************************

## R environment
library(downloader)     ## If needed: install.packages("downloader")

## Downloading and unzipping the zip-file in working directory
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
download(url, dest="dataset.zip", mode = "wb")  ## Download file from site 
unzip ("dataset.zip",exdir = ".")               ## Unzip the compressed downloded file
rm(url)                                         ## Clean up 

## ******************************************************************************************
## 1. Merges the training and the test sets to create one data set.
      
## Read training set files
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "")       
Activity_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "")    
data_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")

## labeling the training set columns
names(subject_train) <- list("Subject")
names(Activity_train) <- list("Activity")
colNames <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, sep = "")  ## read the data labels
colNames <- gsub("[^[:alnum:] ]", "", colNames[,2])   ## Remove non-alphanumerical chars
names(data_train) <- colNames

train_set <- cbind(subject_train, Activity_train, data_train)  ## concatenate the training sets columns
## Clean up training set
rm(subject_train); rm(Activity_train); rm(data_train) 

## Read test set files
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "")      
Activity_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "")     
data_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")

## labeling the test set columns
names(subject_test) <- list("Subject")
names(Activity_test) <- list("Activity")
names(data_test) <- colNames

test_set <- cbind(subject_test, Activity_test, data_test)  ## concatenate the test sets column 
## Clean up test set
rm(subject_test); rm(Activity_test); rm(data_test) 

## Concatenating in to a Total set <- training+test
Dataset <- rbind(train_set, test_set)  ## concatenate the rows into single dataset
## Clean up
rm(train_set); rm(test_set) 

## ******************************************************************************************
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 4. Appropriately labels the data set with descriptive variable names.

## select out some columns/measures for body means - se ReadMe - 
Keep <- grep("tBodyAccJerkMean", colNames)      ## Select column containing "tBodyAccJerkMean"
Keep_tmp <- grep("tBodyGyroMean", colNames)     ## Select column containing "tBodyGyroMean"
Keep <- append(Keep, Keep_tmp)                  ## append to a single keep list
Keep_tmp <- grep("tBodyGyroJerkMean", colNames) ## Select column containing "tBodyGyroJerkMean"
Keep <- append(Keep, Keep_tmp)                  ## append to a single keep list
Keep <- append(c(1, 2), Keep)                   ## append first columns to a single keep list
rm(Keep_tmp)                                    ## remove temp keep list
TidyData_tmp <- Dataset[, Keep ]                ## Create a temporary tidy set with selected columns
names(TidyData_tmp) <- c("Subject", "Activity", "BodyAccJerkMean", "BodyGyroMean", "BodyGyroJerkMean" )
rm(Keep); rm(colNames)                          ## Clean up

## ******************************************************************************************
## 3. Uses descriptive activity names to name the activities in the data set
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

activityLabels <- as.list(read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")) ## Read activity labels from file
TidyData <- data.frame(SubjectId=numeric(180), Activity=NA, BodyAccJerkMean=numeric(180), BodyGyroMean=numeric(180), BodyGyroJerkMean=numeric(180) )  ## create empty dataframe
dfRow = 0                           ## Initiate row counter for the tidy dataset
for (subjectNo in 1:30 ) {          ## Subject loop
      for (activityNo in 1:6 ) {    ## Activity loop
            tmp <- filter(TidyData_tmp, Subject == subjectNo, Activity == activityNo)     ## Filter out one subject at a time
            dfRow <- (dfRow + 1 )                                                         ## Row counter for dataframe
            TidyData$SubjectId[dfRow]          <- subjectNo                                 ## Add subject to tidy dataset
            TidyData$Activity[dfRow]         <- as.character(activityLabels$V2[activityNo]) ## Add activity to tidy dataset with describing name taken from activity label file
            TidyData$BodyAccJerkMean[dfRow]  <- mean(tmp$BodyAccJerkMean)                 ## Add the Calculated BodyAccJerkMean to tidy dataset
            TidyData$BodyGyroMean[dfRow]     <- mean(tmp$BodyGyroMean)                    ## Add the Calculated BodyGyroMean to tidy dataset
            TidyData$BodyGyroJerkMean[dfRow] <- mean(tmp$BodyGyroJerkMean)                ## Add the Calculated BodyGyroJerkMean to tidy dataset
      }  ## End - Activity loop
}  ## End - Subject loop
rm(dfRow); rm(activityLabels) ; rm(subjectNo); rm(activityNo); rm(tmp); rm(TidyData_tmp)  ## Clean up

## ******************************************************************************************
## X. Please upload your data set as a txt file
write.table(TidyData, "./TidyData.txt", row.name=FALSE)


## *************************End of run_analysis.R *********************************